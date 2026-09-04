;; Copyright (c) F4MES
;;
;; Sends track structure and playback position to a QLC+ Track view.
;;
;; The whole track is analysed once when it loads: bass energy per beat is read
;; straight out of rekordbox's own waveform analysis, breaks/builds/drops are
;; derived from it, and the result is pushed to QLC+ in a single message. QLC+
;; then looks up the current beat locally, so nothing has to be sent per beat -
;; only a lightweight position update.
;;
;; Because the analysis covers the entire track (not just what has played), the
;; ramp into a drop can be timed in advance instead of reacting after the fact.

(ns beat-link-trigger.qlc
  (:require [cheshire.core :as json]
            [taoensso.timbre :as timbre])
  (:import [java.net Socket]
           [java.io PrintWriter]
           [org.deepsymmetry.beatlink.data WaveformFinder BeatGridFinder]))

;; ---------------------------------------------------------------------------
;; Connection
;; ---------------------------------------------------------------------------

(defonce ^:private config (atom {:host "127.0.0.1" :port 9998}))

(defn configure!
  "Point the sender at a different QLC+ host/port."
  [host port]
  (swap! config assoc :host host :port port))

(defonce ^:private conn (atom nil))

(defn disconnect! []
  (when-let [{:keys [^Socket socket ^PrintWriter writer]} @conn]
    (try (.close writer) (catch Exception _ nil))
    (try (.close socket) (catch Exception _ nil))
    (reset! conn nil)))

(defn- writer!
  "Return a live PrintWriter to QLC+, connecting if needed. nil if unreachable."
  ^PrintWriter []
  (or (:writer @conn)
      (let [{:keys [host port]} @config]
        (try
          (let [sock (Socket. ^String host (int port))
                wr   (PrintWriter. (.getOutputStream sock) true)]
            (reset! conn {:socket sock :writer wr})
            (timbre/info "QLC+ connected on" host port)
            wr)
          (catch Exception _
            (timbre/warn "QLC+ not reachable on" host port
                         "- is the Track view running?")
            nil)))))

(defn- send-json!
  "Send one JSON object as a single line. Drops the connection on error so the
  next call reconnects."
  [m]
  (when-let [wr (writer!)]
    (try
      (.println wr ^String (json/generate-string m))
      (when (.checkError wr)
        (disconnect!))
      (catch Exception _
        (disconnect!)))))

;; ---------------------------------------------------------------------------
;; Tuning - calibrate against your own library
;; ---------------------------------------------------------------------------

(defonce tuning
  (atom {:quiet-ratio     0.28  ; bass below this fraction of reference = quiet
         :min-break-beats 8     ; shorter quiet passages are not a break
         :ramp-beats      32    ; how long the build ramp runs before a drop
         :window-frames   12    ; half-frames averaged per beat (~80 ms)
         :reference-pct   90    ; percentile treated as "full bass"
         :drop-hold-beats 32})) ; how long the drop state lasts before normal

(def ^:private half-frames-per-ms
  "Waveform detail carries one segment per half-frame = 1/150 second."
  0.15)

;; ---------------------------------------------------------------------------
;; Bass extraction
;;
;; The waveform API has changed across beat-link versions and differs between
;; three-band and RGB analyses, so each strategy is tried in turn rather than
;; assuming one signature.
;; ---------------------------------------------------------------------------

(defn- segment-bass
  "Bass energy 0.0-1.0 for a single waveform segment."
  [detail segment]
  (or
    ;; three-band analysis: band 0 is the low band
    (try
      (let [h (.segmentHeight detail (int segment) (int 0))]
        (when (number? h) (/ (double h) 31.0)))
      (catch Throwable _ nil))
    ;; RGB analysis: low frequencies dominate the red channel
    (try
      (let [c (.segmentColor detail (int segment) (int 1))]
        (/ (double (.getRed c)) 255.0))
      (catch Throwable _ nil))
    ;; last resort: overall amplitude (cannot separate bass from total energy)
    (try
      (/ (double (.segmentHeight detail (int segment) (int 1))) 31.0)
      (catch Throwable _ 0.0))
    0.0))

(defn- bass-at-beat
  [detail grid beat]
  (try
    (let [ms     (.getTimeWithinTrack grid (int beat))
          start  (long (* ms half-frames-per-ms))
          n      (:window-frames @tuning)
          frames (.getFrameCount detail)
          idxs   (filter #(and (>= % 0) (< % frames)) (range start (+ start n)))]
      (if (seq idxs)
        (/ (reduce + (map #(segment-bass detail %) idxs)) (count idxs))
        0.0))
    (catch Throwable _ 0.0)))

(defn bass-curve
  "Bass energy per beat. Index 0 is beat 1."
  [detail grid]
  (mapv #(bass-at-beat detail grid %)
        (range 1 (inc (.getBeatCount grid)))))

;; ---------------------------------------------------------------------------
;; Structure detection
;; ---------------------------------------------------------------------------

(defn- percentile [coll pct]
  (let [s (vec (sort coll))]
    (if (seq s)
      (nth s (min (dec (count s)) (long (* (/ pct 100.0) (count s)))))
      0.0)))

(defn find-breaks
  "Contiguous quiet passages as [{:from b :to b}], 1-indexed, :to inclusive.
  Passages shorter than :min-break-beats are discarded - that filter is the main
  defence against every bass gap in a verse firing a scene change."
  [curve]
  (let [t     @tuning
        limit (* (percentile curve (:reference-pct t)) (:quiet-ratio t))
        quiet (mapv #(< % limit) curve)]
    (->> (range (count quiet))
         (partition-by #(nth quiet %))
         (filter #(nth quiet (first %)))
         (map (fn [run] {:from (inc (first run)) :to (inc (last run))}))
         (filter #(>= (inc (- (:to %) (:from %))) (:min-break-beats t)))
         vec)))

(defn build-markers
  "State-change points for the whole track, sorted by beat.

  QLC+ reads these as: the state at beat B is the type of the latest marker at
  or before B, defaulting to \"normal\". So moving a marker in QLC+ moves
  exactly where the lighting changes."
  [curve]
  (let [t     @tuning
        total (count curve)]
    (->> (find-breaks curve)
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
;; Sending
;; ---------------------------------------------------------------------------

(defn- finders []
  [(WaveformFinder/getInstance) (BeatGridFinder/getInstance)])

(defn send-track!
  "Analyse the track on `player` and push the whole plan to QLC+.
  Call this when a track loads (waveform available)."
  ([player] (send-track! player nil))
  ([player title]
   (try
     (let [[wf bg] (finders)
           detail  (.getLatestDetailFor wf (int player))
           grid    (.getLatestBeatGridFor bg (int player))]
       (if (and detail grid)
         (let [curve   (bass-curve detail grid)
               markers (build-markers curve)]
           (send-json! {:evt      "track"
                        :title    (or title (str "Player " player))
                        :bpm      0.0   ; QLC+ gets live tempo from Ableton Link
                        :beats    (count curve)
                        :waveform (mapv #(int (* 255 (max 0.0 (min 1.0 %)))) curve)
                        :markers  markers})
           (timbre/info "QLC+ track sent - player" player
                        (count curve) "beats," (count markers) "markers"))
         (timbre/info "Waveform or beat grid not ready yet for player" player)))
     (catch Throwable e
       (timbre/error e "Failed to send track for player" player)))))

(defn send-position!
  "Push the current beat to QLC+. Cheap - safe to call on every beat and on
  every tracked update."
  [beat playing?]
  (when (and beat (pos? (int beat)))
    (send-json! {:evt "pos" :beat (int beat) :playing (boolean playing?)})))

;; ---------------------------------------------------------------------------
;; Wiring in BLT - paste these into the expression editors
;;
;;   Global Setup Expression:
;;     (require '[beat-link-trigger.qlc :as qlc])
;;
;;   Global Shutdown Expression:
;;     (beat-link-trigger.qlc/disconnect!)
;;
;;   Trigger -> Beat Expression:
;;     (beat-link-trigger.qlc/send-position! beat-number true)
;;
;;   Trigger -> Tracked Update Expression:
;;     (beat-link-trigger.qlc/send-position! beat-number playing?)
;;
;;   Trigger -> Activation Expression (or wherever you detect a new track):
;;     (beat-link-trigger.qlc/send-track! device-number track-title)
;;
;; Tracked Update is the sync authority: when the DJ jumps to a hot cue or
;; starts exactly on a beat, the beat packet is sometimes missed, and the
;; ~5x/second tracked update fills that gap. Because the plan is indexed by
;; absolute beat number there is no accumulated state to fall out of sync.
;;
;; Calibration - run before hooking up any lights:
;;
;;   (let [[wf bg] [(org.deepsymmetry.beatlink.data.WaveformFinder/getInstance)
;;                  (org.deepsymmetry.beatlink.data.BeatGridFinder/getInstance)]
;;         detail  (.getLatestDetailFor wf 1)
;;         grid    (.getLatestBeatGridFor bg 1)
;;         curve   (beat-link-trigger.qlc/bass-curve detail grid)]
;;     (println "breaks:" (beat-link-trigger.qlc/find-breaks curve)))
;;
;; Play a track you know well and check the breaks land where you know they are.
;; Adjust :quiet-ratio and :min-break-beats in `tuning` accordingly.
;; ---------------------------------------------------------------------------
