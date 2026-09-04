;; ---------------------------------------------------------------------------
;;  QLC+ Track view link  --  PASTE THIS WHOLE FILE INTO BLT "Shared Functions"
;; ---------------------------------------------------------------------------
;;
;;  Sends the playing track's title, waveform and live position to QLC+, which
;;  listens on port 9998. Nothing is sent per beat except a tiny position
;;  message - the whole track is analysed once when it loads.
;;
;;  There is no ns form here on purpose: Shared Functions already runs inside a
;;  namespace, so everything below simply becomes available to the other
;;  expression editors.
;;
;;  Wiring (see the bottom of this file for the exact two lines you need).
;; ---------------------------------------------------------------------------

(require '[cheshire.core :as json])
(import '[java.net Socket]
        '[java.io PrintWriter])

;; ---------------------------------------------------------------------------
;;  Where QLC+ is. Both run on the lighting PC, so localhost.
;; ---------------------------------------------------------------------------

(def qlc-host "127.0.0.1")
(def qlc-port 9998)

(defonce qlc-conn (atom nil))

(defn qlc-disconnect! []
  (when-let [{:keys [socket writer]} @qlc-conn]
    (try (.close writer) (catch Exception _ nil))
    (try (.close socket) (catch Exception _ nil))
    (reset! qlc-conn nil)))

(defn qlc-writer!
  "Live PrintWriter to QLC+, connecting on demand. nil if QLC+ isn't there."
  []
  (or (:writer @qlc-conn)
      (try
        (let [sock (Socket. qlc-host (int qlc-port))
              wr   (PrintWriter. (.getOutputStream sock) true)]
          (reset! qlc-conn {:socket sock :writer wr})
          (timbre/info "QLC+ connected on" qlc-host qlc-port)
          wr)
        (catch Exception _
          (timbre/warn "QLC+ not reachable on" qlc-host qlc-port
                       "- is QLC+ running with the Track page?")
          nil))))

(defn qlc-send!
  "Send one JSON object as a single line. Drops the socket on error so the next
  call reconnects by itself."
  [m]
  (when-let [wr (qlc-writer!)]
    (try
      (.println wr (json/generate-string m))
      (when (.checkError wr)
        (qlc-disconnect!))
      (catch Exception _
        (qlc-disconnect!)))))

;; ---------------------------------------------------------------------------
;;  STEP 1 - prove the link before anything else.
;;  Run (qlc-test!) and the QLC+ Track page must switch to "BLT connected".
;; ---------------------------------------------------------------------------

(defn qlc-test! []
  (qlc-send! {:evt      "track"
              :title    "TEST FROM BLT"
              :bpm      128.0
              :beats    64
              :waveform (vec (repeatedly 64 #(rand-int 256)))
              :markers  [{:beat 17 :type "break"}
                         {:beat 33 :type "drop"}]})
  (qlc-send! {:evt "pos" :beat 1 :playing true})
  "sent - check the QLC+ Track page")

;; ---------------------------------------------------------------------------
;;  Tuning for the break/drop detection. Only matters once the waveform works.
;; ---------------------------------------------------------------------------

(defonce qlc-tuning
  (atom {:quiet-ratio     0.28   ; bass below this share of reference = quiet
         :min-break-beats 8      ; shorter quiet passages are not a break
         :ramp-beats      32     ; how long the build runs before the drop
         :window-frames   12     ; half-frames averaged per beat (~80 ms)
         :reference-pct   90     ; percentile treated as "full bass"
         :drop-hold-beats 32}))  ; how long the drop state lasts

(def qlc-half-frames-per-ms 0.15)  ; waveform detail = 1 segment per 1/150 s

;; ---------------------------------------------------------------------------
;;  Reading bass energy out of rekordbox's own analysis.
;;
;;  The API is segmentHeight(segment, scale) / segmentColor(segment, scale),
;;  where the second argument is how many segments to average into one pixel
;;  column - NOT a frequency band. There is no band accessor at all.
;;
;;  Colour (NXS2) waveforms encode frequency content in the colour channels, and
;;  the red channel tracks the low end, so there we get a genuine bass reading.
;;  Monochrome blue waveforms only carry overall amplitude, so on those this
;;  cannot tell a bass drop from a general drop in energy - the breaks it finds
;;  will be less precise. The isColor field tells us which one we have.
;; ---------------------------------------------------------------------------

(defn qlc-segment-bass [detail segment]
  (try
    (if (.isColor detail)
      (/ (double (.getRed (.segmentColor detail (int segment) (int 1)))) 255.0)
      (/ (double (.segmentHeight detail (int segment) (int 1))) 31.0))
    (catch Throwable _ 0.0)))

(defn qlc-bass-at-beat [detail grid beat]
  (try
    (let [ms     (.getTimeWithinTrack grid (int beat))
          start  (long (* ms qlc-half-frames-per-ms))
          n      (:window-frames @qlc-tuning)
          frames (.getFrameCount detail)
          idxs   (filter #(and (>= % 0) (< % frames)) (range start (+ start n)))]
      (if (seq idxs)
        (/ (reduce + (map #(qlc-segment-bass detail %) idxs)) (count idxs))
        0.0))
    (catch Throwable _ 0.0)))

(defn qlc-bass-curve [detail grid]
  (mapv #(qlc-bass-at-beat detail grid %)
        (range 1 (inc (.getBeatCount grid)))))

;; ---------------------------------------------------------------------------
;;  Structure detection
;; ---------------------------------------------------------------------------

(defn qlc-percentile [coll pct]
  (let [s (vec (sort coll))]
    (if (seq s)
      (nth s (min (dec (count s)) (long (* (/ pct 100.0) (count s)))))
      0.0)))

(defn qlc-find-breaks
  "Quiet passages as [{:from b :to b}], 1-indexed, :to inclusive. Passages
  shorter than :min-break-beats are dropped - that filter is what stops every
  bass gap in a verse from firing a scene change."
  [curve]
  (let [t     @qlc-tuning
        limit (* (qlc-percentile curve (:reference-pct t)) (:quiet-ratio t))
        quiet (mapv #(< % limit) curve)]
    (->> (range (count quiet))
         (partition-by #(nth quiet %))
         (filter #(nth quiet (first %)))
         (map (fn [run] {:from (inc (first run)) :to (inc (last run))}))
         (filter #(>= (inc (- (:to %) (:from %))) (:min-break-beats t)))
         vec)))

(defn qlc-build-markers
  "State-change points. QLC+ reads these as: the state at beat B is the type of
  the latest marker at or before B, default \"normal\". So dragging a marker in
  QLC+ moves exactly where the lighting changes."
  [curve]
  (let [t     @qlc-tuning
        total (count curve)]
    (->> (qlc-find-breaks curve)
         (mapcat
           (fn [{:keys [from to]}]
             (let [drop-beat  (inc to)
                   ramp-len   (min (:ramp-beats t) (inc (- to from)))
                   ramp-start (max from (- drop-beat ramp-len))
                   after-drop (+ drop-beat (:drop-hold-beats t))]
               (cond-> [{:beat from :type "break"}]
                 (> ramp-start from)   (conj {:beat ramp-start :type "build"})
                 (<= drop-beat total)  (conj {:beat drop-beat :type "drop"})
                 (<= after-drop total) (conj {:beat after-drop :type "normal"})))))
         (sort-by :beat)
         vec)))

;; ---------------------------------------------------------------------------
;;  Sending the track
;; ---------------------------------------------------------------------------

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
        (let [curve (qlc-bass-curve detail grid)]
          (qlc-send! {:evt      "track"
                      :title    (qlc-title player)
                      :bpm      0.0        ; QLC+ takes live tempo from Link
                      :beats    (count curve)
                      ;; exact track length - beat-link knows the half-frame count
                      :duration (try (.getTotalTime detail) (catch Throwable _ 0))
                      :waveform (mapv #(int (* 255 (max 0.0 (min 1.0 %)))) curve)
                      :markers  (qlc-build-markers curve)})
          (timbre/info "QLC+ track sent -" (count curve) "beats,"
                       (if (.isColor detail)
                         "colour waveform (real bass reading)"
                         "monochrome waveform (amplitude only)"))
          true)
        (do (timbre/info "Waveform/beat grid not ready for player" player)
            false)))
    (catch Throwable e
      (timbre/error e "QLC+ send-track failed for player" player)
      false)))

(defn qlc-send-position! [beat playing?]
  (when (and beat (pos? (int beat)))
    (qlc-send! {:evt "pos" :beat (int beat) :playing (boolean playing?)})))

;; ---------------------------------------------------------------------------
;;  Track change detection, so you only need one line in Tracked Update
;; ---------------------------------------------------------------------------

(defonce qlc-last-track (atom {}))

(defn qlc-maybe-send-track! [player]
  (let [sig (qlc-title player)]
    (when (not= sig (get @qlc-last-track player))
      (when (qlc-send-track! player)
        (swap! qlc-last-track assoc player sig)))))

(defn qlc-tracked-update!
  "Two arities so the wiring does not depend on a binding that may or may not be
  offered in your expression's help text. Pass the playing flag if you have one."
  ([player beat]
   (qlc-tracked-update! player beat true))
  ([player beat playing?]
   (qlc-maybe-send-track! player)
   (qlc-send-position! beat playing?)))

(defn qlc-beat! [beat]
  (qlc-send-position! beat true))

;; ---------------------------------------------------------------------------
;;  WIRING - two one-liners, that's all
;;
;;  This file goes in:  Triggers window  ->  Triggers  ->  Edit Shared Functions
;;
;;  The two expressions below live on a single trigger, reached by
;;  right-clicking (or control-clicking) that trigger's row to open its context
;;  menu. Set the trigger's Watch menu to Master Player.
;;
;;    Beat Expression:
;;      (qlc-beat! beat-number)
;;
;;    Tracked Update Expression:
;;      (qlc-tracked-update! device-number beat-number)
;;
;;  Leave the trigger's Enabled menu at Never and its Message menu at Custom:
;;  we only want our own code to run, not MIDI on play/stop.
;;
;;  (If the Tracked Update help text lists a playing flag, you can pass it as a
;;  third argument to get an accurate PLAYING/PAUSED readout in QLC+. Without
;;  it the readout simply always says playing, which is harmless.)
;;
;;  Tracked Update is the sync authority. When the DJ jumps to a hot cue or
;;  starts exactly on a beat, the beat packet is sometimes missed; the tracked
;;  update runs about five times a second and fills that gap. Because the plan
;;  is indexed by absolute beat number there is no accumulated state that can
;;  drift out of sync.
;;
;;  BRING IT UP IN THIS ORDER, and stop at the first thing that fails:
;;
;;    1. QLC+ open on the Track page. It should say "waiting for BLT", port 9998.
;;    2. Run (qlc-test!) from any BLT expression editor.
;;       -> QLC+ must switch to "BLT connected" and draw a random test waveform.
;;       If not, it is the port or the firewall. Fix that before going further.
;;    3. Add the two lines above, load a track on a CDJ.
;;       -> Title and the real waveform appear.
;;    4. Press play.
;;       -> The white pin follows the CDJ.
;;
;;  Only once all four work is it worth looking at whether the breaks and drops
;;  land in the right places. To check that without touching any lights:
;;
;;    (let [wf     (org.deepsymmetry.beatlink.data.WaveformFinder/getInstance)
;;          bg     (org.deepsymmetry.beatlink.data.BeatGridFinder/getInstance)
;;          detail (.getLatestDetailFor wf 1)
;;          grid   (.getLatestBeatGridFor bg 1)
;;          curve  (qlc-bass-curve detail grid)]
;;      (println "breaks:" (qlc-find-breaks curve)))
;;
;;  Play a track you know well and see whether the breaks land where you know
;;  they are. Then tune it live, without re-pasting anything:
;;
;;    (swap! qlc-tuning assoc :quiet-ratio 0.22 :min-break-beats 12)
;; ---------------------------------------------------------------------------
