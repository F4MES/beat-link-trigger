;; ---------------------------------------------------------------------------
;;  QLC+ Track view link  --  Shared Functions
;;
;;  MAALING   pr. beat: level (lydstyrke), low (bas), kick (puls: hvor meget
;;            hoejere og roedere beatets attack er end resten af beatet).
;;  TAKTER    beatgrid'et kender downbeats. Frasefasen fittes blandt 8 takter.
;;  GRAENSER  overgange maales paa 2, 4 og 8 takters skala, saa baade trin og
;;            ramper fanges. Taersklen tilpasser sig laengden. Ingen sektion
;;            maa vaere over 32 takter - laengere deles ved staerkeste overgang.
;;            Hver graense flyttes til taktlinjen (+-2) hvor kicket skifter mest.
;;  NIVEAU    kick fra -> break. Kick tilbage efter break MED hoej energi -> drop.
;;            Ellers groove, medmindre energien loefter sig til fuld.
;;  BUILD     straekkes bagud fra drop'et saa laenge energien stiger (8-32 takter).
;; ---------------------------------------------------------------------------

(require '[cheshire.core :as json])
(import '[java.net Socket]
        '[java.io PrintWriter])

;; ---------------------------------------------------------------- forbindelse

(def qlc-host "127.0.0.1")
(def qlc-port 9998)

(defonce qlc-conn (atom nil))
(def qlc-last-track (atom {}))

(defn qlc-disconnect! []
  (when-let [{:keys [socket writer]} @qlc-conn]
    (try (.close writer) (catch Exception _ nil))
    (try (.close socket) (catch Exception _ nil))
    (reset! qlc-conn nil)))

(defn qlc-writer! []
  (or (:writer @qlc-conn)
      (try
        (let [sock (Socket. qlc-host (int qlc-port))
              wr   (PrintWriter. (.getOutputStream sock) true)]
          (reset! qlc-conn {:socket sock :writer wr})
          (timbre/info "QLC+ connected on" qlc-host qlc-port)
          wr)
        (catch Exception _ nil))))

(defn qlc-send! [m]
  (if-let [wr (qlc-writer!)]
    (try
      (.println wr (json/generate-string m))
      (if (.checkError wr) (do (qlc-disconnect!) false) true)
      (catch Exception _ (qlc-disconnect!) false))
    false))

(defn qlc-test! []
  (qlc-send! {:evt "track" :title "TEST FROM BLT" :bpm 128.0 :beats 64
              :waveform (vec (repeatedly 64 #(rand-int 256)))
              :markers [{:beat 17 :type "break"} {:beat 33 :type "drop"}]})
  (qlc-send! {:evt "pos" :beat 1 :playing true}))

;; ---------------------------------------------------------------- tuning

(def qlc-tuning
  (atom {:min-section-bars 8      ; korteste sektion
         :max-section-bars 32     ; laengste sektion - laengere deles
         :bars-per-section 24     ; sigtepunkt: en sektion pr. saa mange takter
         :build-bars       8      ; build er mindst saa mange takter ...
         :build-max-bars   32     ; ... og hoejst saa mange, mens energien stiger
         :smooth-beats     4      ; glatning: en takt
         :kick-on          0.40   ; sektionens kick over dette = kick er paa
         :loud-on          0.65   ; ... eller energi over dette (hedge)
         :drop-min         0.60   ; kick tilbage efter break er drop fra denne energi
         :abs-full         0.85   ; energi-fallback: herover ...
         :drop-rise        0.20   ; ... og mindst saa stort loeft = drop
         :refine-bars      2      ; flyt graenser op til saa mange takter
         :refine-gain      1.25   ; ... hvis kick-skiftet der er saa meget staerkere
         :attack-frac      6}))   ; attack-vinduet = 1/6 af beatet (~80 ms)

(defn qlc-opt [k dflt] (get @qlc-tuning k dflt))

(def qlc-half-frames-per-ms 0.15)

(defn qlc-min-beats []   (* 4 (qlc-opt :min-section-bars 8)))
(defn qlc-max-beats []   (* 4 (qlc-opt :max-section-bars 32)))
(defn qlc-build-beats [] (* 4 (qlc-opt :build-bars 8)))

;; ---------------------------------------------------------------- hjaelpere

(defn qlc-percentile [coll pct]
  (let [s (vec (sort coll))]
    (if (seq s)
      (nth s (min (dec (count s)) (long (* (/ pct 100.0) (count s)))))
      0.0)))

(defn qlc-normalize-to [curve ref]
  (if (> ref 1e-6)
    (mapv #(min 1.0 (/ % ref)) curve)
    (mapv (constantly 0.0) curve)))

(defn qlc-smooth [curve n]
  (let [v (vec curve) c (count v) half (quot (max 1 n) 2)]
    (mapv (fn [i]
            (let [lo (max 0 (- i half))
                  hi (min c (+ i half 1))
                  sl (subvec v lo hi)]
              (/ (reduce + sl) (count sl))))
          (range c))))

(defn qlc-mean-range
  "Gennemsnit i det 0-indekserede interval [a b)."
  [v a b]
  (let [c (count v) a (max 0 a) b (min c b)]
    (if (< a b) (/ (reduce + (subvec v a b)) (- b a)) 0.0)))

(defn qlc-step
  "Hvor meget kurven skifter ved i, maalt over w beats paa hver side."
  [v i w]
  (Math/abs (- (qlc-mean-range v i (+ i w))
               (qlc-mean-range v (- i w) i))))

;; ---------------------------------------------------------------- maaling

(defn qlc-measure-beat
  "Et beat -> {:level :low :kick}. kick = hvor meget mere bas-energi der er i
   beatets attack-vindue end i resten. Tre vinduer proeves, saa et beatgrid der
   ligger lidt skaevt ikke skjuler kicket."
  [detail grid beat total frames]
  (let [ms0    (.getTimeWithinTrack grid (int beat))
        ms1    (if (< beat total)
                 (.getTimeWithinTrack grid (int (inc beat)))
                 (+ ms0 470))
        s0     (long (* ms0 qlc-half-frames-per-ms))
        s1     (min frames (max (inc s0) (long (* ms1 qlc-half-frames-per-ms))))
        n      (max 1 (- s1 s0))
        attack (max 2 (quot n (qlc-opt :attack-frac 6)))
        col?   (.-isColor detail)
        segs   (mapv (fn [i]
                       (let [h     (double (.segmentHeight detail (int i) (int 1)))
                             share (if col?
                                     (let [c  (.segmentColor detail (int i) (int 1))
                                           rr (double (.getRed c))
                                           gg (double (.getGreen c))
                                           bb (double (.getBlue c))]
                                       (/ rr (max 1.0 (+ rr gg bb))))
                                     1.0)]
                         [h (* h share)]))
                     (range s0 s1))
        hs     (mapv first segs)
        lws    (mapv second segs)
        sumH   (reduce + hs)
        sumLow (reduce + lws)
        win    (fn [o] (qlc-mean-range lws o (+ o attack)))
        a      (max (win 0) (win (quot attack 2)) (win attack))
        r      (/ (- sumLow (* a attack)) (max 1 (- n attack)))]
    {:level (/ sumH (* 31.0 n))
     :low   (/ sumLow (* 31.0 n))
     :kick  (max 0.0 (/ (- a r) 31.0))}))

(defn qlc-curves [detail grid]
  (let [total  (.-beatCount grid)
        frames (.getFrameCount detail)
        ms     (mapv #(qlc-measure-beat detail grid % total frames)
                     (range 1 (inc total)))
        level  (mapv :level ms)
        low    (mapv :low ms)
        kick   (mapv :kick ms)
        ln     (qlc-normalize-to level (qlc-percentile level 95))
        wn     (qlc-normalize-to low   (qlc-percentile low 75))
        kn     (qlc-normalize-to kick  (qlc-percentile kick 90))
        sm     (qlc-opt :smooth-beats 4)]
    {:display ln
     :energy  (qlc-smooth (mapv min ln wn) sm)
     :kick    (qlc-smooth kn sm)}))

;; ---------------------------------------------------------------- takter

(defn qlc-downbeat
  "0-indekseret index paa foerste beat der er 1-slaget i en takt."
  [grid total]
  (or (first (filter (fn [i] (= 1 (.getBeatWithinBar grid (int (inc i)))))
                     (range (min total 8))))
      0))

(defn qlc-transition
  "Overgangsstyrke ved i, paa 2, 4 og 8 takters skala - det stoerste taeller,
   saa baade skarpe trin og lange ramper fanges. Kick vaegter dobbelt."
  [energy kick i]
  (apply max (map (fn [w] (+ (* 2.0 (qlc-step kick i w)) (qlc-step energy i w)))
                  [8 16 32])))

(defn qlc-phrase-lines [n downbeat phase]
  (->> (range (+ downbeat (* 4 phase)) n 32)
       (filter #(and (>= % 16) (<= % (- n 16))))
       vec))

(defn qlc-phrase-phase
  "Hvilken af de 8 takter i frasen der bedst flugter med nummerets skift."
  [energy kick downbeat]
  (let [n (count energy)]
    (if (< n 64)
      0
      (apply max-key
             (fn [k] (reduce + (map #(qlc-transition energy kick %)
                                    (qlc-phrase-lines n downbeat k))))
             (range 8)))))

;; ---------------------------------------------------------------- sektioner

(def qlc-tier-name {0 "break" 1 "normal" 2 "drop"})

(defn qlc-seg [energy kick a b]
  {:from   (inc a)
   :to     b
   :energy (qlc-mean-range energy a b)
   :kick   (qlc-mean-range kick a b)})

(defn qlc-segs-from-cuts [energy kick cuts]
  (mapv (fn [[a b]] (qlc-seg energy kick a b))
        (partition 2 1 (concat [0] cuts [(count energy)]))))

(defn qlc-absorb-short
  "Opsug sektioner kortere end minlen i den nabo de ligner mest."
  [segs minlen energy kick]
  (loop [ss (vec segs) guard 0]
    (if (or (> guard 300) (<= (count ss) 1))
      ss
      (let [i (first (keep-indexed
                       (fn [i s] (when (< (inc (- (:to s) (:from s))) minlen) i))
                       ss))]
        (if (nil? i)
          ss
          (let [dist (fn [x y] (+ (Math/abs (- (:kick x) (:kick y)))
                                  (Math/abs (- (:energy x) (:energy y)))))
                tgt  (cond
                       (zero? i) 1
                       (= i (dec (count ss))) (dec i)
                       :else (if (< (dist (nth ss (dec i)) (nth ss i))
                                    (dist (nth ss (inc i)) (nth ss i)))
                               (dec i) (inc i)))
                lo   (min i tgt)
                hi   (max i tgt)
                m    (qlc-seg energy kick (dec (:from (nth ss lo))) (:to (nth ss hi)))]
            (recur (vec (concat (subvec ss 0 lo) [m] (subvec ss (inc hi))))
                   (inc guard))))))))

(defn qlc-target-sections [beats]
  (let [bars (quot beats 4)]
    (max 3 (min 12 (quot bars (qlc-opt :bars-per-section 24))))))

(defn qlc-best-cuts
  "Vaelg den taerskel der giver et fornuftigt antal sektioner for laengden."
  [energy kick lines]
  (let [scores (mapv (fn [i] {:idx i :score (qlc-transition energy kick i)}) lines)
        ref    (qlc-percentile (mapv :score scores) 90)
        target (qlc-target-sections (count energy))
        tries  (mapv (fn [r]
                       (let [cuts (->> scores
                                       (filter #(> (:score %) (* ref r)))
                                       (mapv :idx))
                             segs (qlc-absorb-short (qlc-segs-from-cuts energy kick cuts)
                                                    (qlc-min-beats) energy kick)]
                         {:ratio r :cuts cuts :n (count segs)}))
                     [0.20 0.28 0.36 0.45 0.55 0.68 0.82])]
    (apply min-key
           (fn [t] (+ (Math/abs (- (:n t) target)) (* 0.01 (:ratio t))))
           tries)))

(defn qlc-split-long
  "Ingen sektion maa vaere laengere end max-beats. Laengere deles ved den
   staerkeste overgang paa en frasegraense inde i den - det er det der sikrer
   at en lang rampe eller et sent drop ikke bliver slugt af en enkelt sektion."
  [cuts energy kick lines]
  (let [n      (count energy)
        maxlen (qlc-max-beats)]
    (loop [cuts (vec (sort (distinct cuts))) guard 0]
      (let [new (for [[a b] (partition 2 1 (concat [0] cuts [n]))
                      :when (> (- b a) maxlen)
                      :let  [inside (filter #(and (>= % (+ a 16)) (<= % (- b 16))) lines)]
                      :when (seq inside)]
                  (apply max-key #(qlc-transition energy kick %) inside))]
        (if (or (empty? new) (> guard 20))
          cuts
          (recur (vec (sort (distinct (concat cuts new)))) (inc guard)))))))

(defn qlc-refine-cuts
  "Flyt hver graense til den taktlinje inden for +-refine-bars hvor kicket
   skifter mest - hvis skiftet der er klart staerkere end paa frasegraensen."
  [cuts kick]
  (let [n    (count kick)
        rb   (qlc-opt :refine-bars 2)
        gain (qlc-opt :refine-gain 1.25)
        sc   (fn [i] (qlc-step kick i 4))]
    (->> cuts
         (mapv (fn [c]
                 (let [cands (->> (range (- rb) (inc rb))
                                  (map #(+ c (* 4 %)))
                                  (filter #(and (>= % 8) (<= % (- n 8)))))
                       best  (if (seq cands) (apply max-key sc cands) c)]
                   (if (> (sc best) (* gain (sc c))) best c))))
         distinct
         sort
         vec)))

(defn qlc-classify
  "Kick fra -> break. Kick tilbage efter break MED hoej energi -> drop.
   Ellers groove, medmindre energien loefter sig til fuld - saa er det drop."
  [segs]
  (let [kt   (qlc-opt :kick-on 0.40)
        lt   (qlc-opt :loud-on 0.65)
        dm   (qlc-opt :drop-min 0.60)
        af   (qlc-opt :abs-full 0.85)
        rise (qlc-opt :drop-rise 0.20)]
    (reduce (fn [acc s]
              (let [prev (peek acc)
                    e    (:energy s)
                    on?  (or (> (:kick s) kt) (> e lt))
                    tier (cond
                           (not on?)                                       0
                           (and prev (= 0 (:tier prev)) (>= e dm))         2
                           (and prev (>= e af) (>= (- e (:energy prev)) rise)) 2
                           :else                                           1)]
                (conj acc (assoc s :tier tier))))
            []
            segs)))

(defn qlc-coalesce
  "Slaa nabo-sektioner med samme tier sammen - men ikke hen over et stort
   energispring, for det er information lyset skal have."
  [segs]
  (reduce (fn [acc s]
            (let [p (peek acc)]
              (if (and p
                       (= (:tier p) (:tier s))
                       (< (Math/abs (- (:energy p) (:energy s))) 0.25))
                (conj (pop acc)
                      (assoc p :to (:to s)
                               :energy (max (:energy p) (:energy s))
                               :kick   (max (:kick p) (:kick s))))
                (conj acc s))))
          []
          segs))

(defn qlc-analyse
  "Hele pipelinen."
  [{:keys [energy kick]} grid]
  (let [n        (count energy)
        downbeat (qlc-downbeat grid n)
        phase    (qlc-phrase-phase energy kick downbeat)
        lines    (qlc-phrase-lines n downbeat phase)
        best     (qlc-best-cuts energy kick lines)
        cuts     (-> (:cuts best)
                     (qlc-split-long energy kick lines)
                     (qlc-refine-cuts kick))
        segs     (-> (qlc-segs-from-cuts energy kick cuts)
                     (qlc-absorb-short (qlc-min-beats) energy kick)
                     qlc-classify
                     qlc-coalesce)]
    {:sections segs :phase phase :downbeat downbeat
     :ratio (:ratio best) :target (qlc-target-sections n) :got (:n best)}))

(defn qlc-build-start
  "0-indekseret start paa build'et foer et drop ved index d: gaa baglaens i
   takter saa laenge energien stiger, mellem build-bars og build-max-bars,
   men aldrig foer lo (start paa den foregaaende sektion)."
  [energy d lo]
  (let [minb (qlc-build-beats)
        maxb (* 4 (qlc-opt :build-max-bars 32))
        rising? (fn [s] (< (+ (qlc-mean-range energy (- s 8) s) 0.03)
                           (qlc-mean-range energy s (+ s 8))))]
    (loop [s (- d minb)]
      (if (and (> (- s 4) lo) (< (- d s) maxb) (rising? s))
        (recur (- s 4))
        (max s lo)))))

(defn qlc-build-markers
  "Sektionsmarkoerer, plus build foran hvert drop."
  [curves grid]
  (let [energy (:energy curves)
        runs   (:sections (qlc-analyse curves grid))
        n      (count runs)]
    (->> (map-indexed vector runs)
         (mapcat
           (fn [[i r]]
             (let [nxt   (when (< (inc i) n) (nth runs (inc i)))
                   drop? (and nxt (= 2 (:tier nxt)))
                   e     (:energy r)
                   lo    (dec (:from r))
                   bs    (when drop? (qlc-build-start energy (dec (:from nxt)) lo))]
               (cond
                 (and drop? (<= bs lo))
                 [{:beat (:from r) :type "build" :energy e}]

                 drop?
                 [{:beat (:from r) :type (qlc-tier-name (:tier r)) :energy e}
                  {:beat (inc bs) :type "build" :energy e}]

                 :else
                 [{:beat (:from r) :type (qlc-tier-name (:tier r)) :energy e}]))))
         (sort-by :beat)
         (partition-by :beat)
         (map last)
         vec)))

;; ---------------------------------------------------------------- afsendelse

(defn qlc-title [player]
  (try
    (let [mf (org.deepsymmetry.beatlink.data.MetadataFinder/getInstance)
          md (.getLatestMetadataFor mf (int player))]
      (if md
        (let [t (.getTitle md)
              a (try (.getName (.getArtist md)) (catch Throwable _ nil))]
          (if (and a (seq a)) (str a " - " t) t))
        (str "Player " player)))
    (catch Throwable _ (str "Player " player))))

(defn qlc-send-track! [player]
  (try
    (let [wf     (org.deepsymmetry.beatlink.data.WaveformFinder/getInstance)
          bg     (org.deepsymmetry.beatlink.data.BeatGridFinder/getInstance)
          detail (.getLatestDetailFor wf (int player))
          grid   (.getLatestBeatGridFor bg (int player))]
      (if (and detail grid)
        (let [curves  (qlc-curves detail grid)
              markers (qlc-build-markers curves grid)
              display (:display curves)
              ok      (qlc-send! {:evt      "track"
                                  :title    (qlc-title player)
                                  :bpm      (try (/ (.getBpm grid (int 1)) 100.0)
                                                 (catch Throwable _ 0.0))
                                  :beats    (count display)
                                  :duration (try (.getTotalTime detail)
                                                 (catch Throwable _ 0))
                                  :waveform (mapv #(int (* 255 (max 0.0 (min 1.0 %))))
                                                  display)
                                  :markers  markers})]
          (try (timbre/info "QLC+ track" (if ok "sent" "FAILED") "-"
                            (count display) "beats," (count markers) "markers")
               (catch Throwable _ nil))
          ok)
        false))
    (catch Throwable e
      (timbre/error e "QLC+ send-track failed for player" player)
      false)))

(defn qlc-send-position! [beat playing?]
  (when (and beat (pos? (int beat)))
    (qlc-send! {:evt "pos" :beat (int beat) :playing (boolean playing?)})))

;; ---------------------------------------------------------------- wiring

(defn qlc-maybe-send-track! [player]
  (let [sig (qlc-title player)]
    (when (not= sig (get @qlc-last-track player))
      (when (qlc-send-track! player)
        (swap! qlc-last-track assoc player sig)))))

(defn qlc-tracked-update!
  ([player beat] (qlc-tracked-update! player beat true))
  ([player beat playing?]
   (qlc-maybe-send-track! player)
   (qlc-send-position! beat playing?)))

(defn qlc-beat! [beat]
  (qlc-send-position! beat true))

;; ---------------------------------------------------------------- vaerktoej

(defn qlc-explain! [player]
  (let [wf     (org.deepsymmetry.beatlink.data.WaveformFinder/getInstance)
        bg     (org.deepsymmetry.beatlink.data.BeatGridFinder/getInstance)
        detail (.getLatestDetailFor wf (int player))
        grid   (.getLatestBeatGridFor bg (int player))
        curves (qlc-curves detail grid)
        {:keys [sections phase downbeat ratio target got]} (qlc-analyse curves grid)
        n      (count (:energy curves))
        bar    (fn [beat] (inc (quot (- (dec beat) downbeat) 4)))]
    (javax.swing.JOptionPane/showMessageDialog
      nil
      (str "beats: " n "   takter: " (quot n 4)
           "   downbeat: beat " (inc downbeat)
           "   frasefase: takt " (inc phase)
           "\ntaerskel: " (format "%.2f" ratio)
           "   maal: " target "   fik: " got
           "\nsektioner: " (count sections) "\n"
           (clojure.string/join
             "\n"
             (map #(format "  %-7s takt %3d-%3d  (%2d)  kick=%.2f  e=%.2f"
                           (qlc-tier-name (:tier %))
                           (bar (:from %)) (bar (inc (:to %)))
                           (quot (inc (- (:to %) (:from %))) 4)
                           (double (:kick %)) (double (:energy %)))
                  sections))))))
