(ns likely.example.gui
  (:require [clojure.string]
            [clojure.core.async :as a]
            [likely.example.book :as book])
  (:import [javax.swing JFrame JPanel JTextField JTextArea
            JList JScrollPane ListSelectionModel ListCellRenderer
            BorderFactory SwingUtilities DefaultListModel JSplitPane]
           [java.awt BorderLayout Dimension Color Font]
           [javax.swing JFrame]
           [java.awt.event WindowAdapter]
           [java.awt.event MouseAdapter KeyAdapter]))

(defonce ^:private *frame (atom nil))



;; Håll koll på vår enda kanal/worker
(defonce ^:private search-ch* (atom nil))

(defn ensure-search-worker!
  "Startar en (1) worker om den inte redan finns.
   apply-results! måste uppdatera GUI:t via EDT."
  [apply-results! work]
  (when (nil? @search-ch*)
    (let [ch (a/chan (a/sliding-buffer 1))] ; behåll bara SENASTE
      (reset! search-ch* ch)
      (a/go-loop []
        (when-let [q (a/<! ch)]
          ;; Kör sökningen i bakgrundstråd (daemon)
          (let [res (a/<! (a/thread (work q)))]
            (apply-results! res))
          (recur))))))

(defn request-search!
  "Kallas från din DocumentListener: skicka in nuvarande text."
  [q apply-results! get-words]
  (ensure-search-worker! apply-results! get-words)
  (a/put! @search-ch* q))     ;; put! är icke-blockerande på GUI-tråden

(defn stop-search-worker!
  "Stäng ner snällt (t.ex. vid fönster-dispose)."
  []
  (when-let [ch @search-ch*]
    (a/close! ch)
    (reset! search-ch* nil)))

;; Kör på EDT utan att blockera (ingen retur)
(defn on-edt!
  "Kör thunk på EDT; om vi redan är på EDT körs den direkt."
  [thunk]                                  ; thunk är (fn [] ...)
  (if (SwingUtilities/isEventDispatchThread)
    (thunk)
    (SwingUtilities/invokeLater ^Runnable thunk)))

(defmacro EDT!
  "Fire-and-forget på EDT."
  [& body]
  `(on-edt! (fn [] ~@body)))

(defmacro EDT??
  "Synkront på EDT, returnerar värdet."
  [& body]
  `(on-edt-sync (fn [] ~@body)))

;; Kör på EDT och vänta (med returvärde)
(defn on-edt-sync
  "Kör thunk på EDT och returnera dess värde; blockar om vi inte är på EDT."
  [thunk]
  (if (SwingUtilities/isEventDispatchThread)
    (thunk)
    (let [result (promise)
          error  (atom nil)]
      (SwingUtilities/invokeAndWait
       (proxy [Runnable] []
         (run []
           (try
             (deliver result (thunk))
             (catch Throwable t
               (reset! error t))))))
      (if @error (throw @error) @result))))


(def debug-area (doto (JTextArea.)
                  (.setEditable false)
                  (.setLineWrap true)
                  (.setWrapStyleWord true)))


(defn append-debug
  [^String text]
  (EDT!
   (.append debug-area (str text "\n"))
   (.setCaretPosition debug-area (.getLength (.getDocument debug-area)))))

(defmacro with-stdout

  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       (let [a# (do ~@body)]
         [a# (str s#)]))))

(with-stdout (println "Hej") (+ 4 5) (println "Hå") 4)

(def search-field (JTextField.))
(def default-font (.getFont search-field))
(def larger-font (Font. (.getName default-font) Font/PLAIN (+ 4 (.getSize default-font))))
(defn multiline-renderer []
  (proxy [javax.swing.JTextArea ListCellRenderer] []
    (getListCellRendererComponent
      [list value index isSelected cellHasFocus]
      (let [area (JTextArea. ^String value)
            width (.getWidth list)
            real-width (if (pos? width) (- width 20) 400)]
        
        ;; layout
        (.setLineWrap area true)
        (.setWrapStyleWord area true)
        (.setEditable area false)
        (.setOpaque area true)
        (.setFont area default-font)
        (.setBorder area (BorderFactory/createEmptyBorder 2 4 2 4))
        

        ;; 💡 Sätt storlek och preferred size
        (.setSize area (Dimension. real-width Integer/MAX_VALUE))
        (.setPreferredSize area (.getPreferredSize area))

        ;; Färg
        (.setBackground area
                        (cond
                          isSelected (.getSelectionBackground list)
                          (even? index) (Color. 240 240 240)
                          :else (Color. 255 255 255)))
        (.setForeground area
                        (if isSelected
                          (.getSelectionForeground list)
                          (.getForeground list)))

        area))))


;; 2. Skapa JList med renderer

(defn make-zebra-jlist [model]
  (let [jlist (JList. model)]
    (.setCellRenderer jlist (multiline-renderer))
    (.setFixedCellHeight jlist -1)      ; tillåter varierande höjd
    ;; 💡 Tvinga renderingen vid resize så att setSize får ny bredd
    (.addComponentListener jlist
      (proxy [java.awt.event.ComponentAdapter] []
        (componentResized [_]
          (.revalidate jlist)
          (.repaint jlist)
          (.updateUI jlist))))
    {:component (JScrollPane. jlist)
     :model model
     :jlist jlist}))


(defn  close-window [frame]
  (proxy [WindowAdapter] []
    (windowClosing [_]
      (if @*frame
        (do
          (println "Disposar fönster. Jag tror jag är (REPL)...")
          (.dispose frame))
        (do
          (println "Avslutar JVM (main)...")
          (System/exit 0))))))





(def search-panel
  (doto (JPanel. (BorderLayout.))
    (.add (javax.swing.JLabel. "Query: ") BorderLayout/WEST)
    (.add search-field BorderLayout/CENTER)))

(def left-list-model (DefaultListModel.))
(def left-list (doto (JList. left-list-model)
                 (.setSelectionMode ListSelectionModel/MULTIPLE_INTERVAL_SELECTION)))
(def left-scroll (doto (JScrollPane. left-list)
                   (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)))
(def left-panel
  (doto (JPanel. (BorderLayout.))
        (.add  (javax.swing.JLabel. "Select words:") BorderLayout/NORTH)
        (.add  left-scroll BorderLayout/CENTER)))

(def middle-list-model (DefaultListModel.))

(def zebra-model (DefaultListModel.))

(defn get-paragraphs [words]

  (->> (mapcat book/refs words)
       (book/deduped-paragraphs-sorted-by-matches
        [:book :chapter :paragraph]
        words)
       (map #(str (:book %) " - " (:chapter %) ":\n" (:text %)))))

(def middle-set
  (let [a (atom [])]
    (add-watch
     a "key"
     (fn [_key _atom _old-state new-state]
       (EDT!
        (let [paragraphs (get-paragraphs new-state)] ;; Det här ska nog ske i en agent som updaterar listan efter sökning är klar
           (.clear middle-list-model)
           (doseq [w new-state]
             (.addElement middle-list-model w))
           (.clear zebra-model)
           (doseq [w paragraphs]
             (.addElement zebra-model w))))))
    a))

(def middle-list (JList. middle-list-model))

(def middle-scroll (JScrollPane. middle-list))

(defn conj-unique [v item]
  (if (some #(= % item) v)
    v
    (conj v item)))

(defn remove-item [v item]
  (vec (remove #(= % item) v)))


(def middle-panel
  (doto (JPanel. (BorderLayout.))
    (.add  (javax.swing.JLabel. "Selected words:") BorderLayout/NORTH)
    (.add  middle-scroll BorderLayout/CENTER)))

(defn update-left [words]
  (EDT!
   (.clear left-list-model)
   (doseq [w words]
     (.addElement left-list-model w))))

#_(Current search innehåller det senaste vi vill söka efter.
           Ska vara nil om ingen sökning sker.
           Om man vill söka efter något, så ersätter man, erästter man nil så söker man.
           Om det man sökt efter inte längre står, så får man söka igen.
           Sökning sker således i en egen tråd, som startas av den som inleder sökning, eftersom denna potentiellt kan få söka många gånger. 
           )


#_(defn get-words [text]
  (let [[r s] (with-stdout
                         (book/search text))]
             (append-debug s)
             (update-left r))
  #_(swap! current-search
           (fn [s]
             (if s
               text)
             (let [[r s] (with-stdout
                           (book/search text))]
               )))
  )

(defn get-words [text]
  (request-search! text
                  (fn [[result debug-text]]
                    (append-debug debug-text)
                    (update-left result))
                  (fn [text] (with-stdout
                           (book/search text append-debug)))))


(defn create-ui []
  (let [frame (JFrame. "Clever search in books")
        root (doto (JPanel. (BorderLayout.))
               (.setPreferredSize (Dimension. 800 600)))
        ;;{:keys [right zebra]} (make-zebra-panel)
        {:keys [component]} (make-zebra-jlist zebra-model)
       
        debug-scroll (JScrollPane. debug-area)
        leftsplit (JSplitPane. JSplitPane/HORIZONTAL_SPLIT left-panel middle-panel)
        rightsplit (JSplitPane. JSplitPane/HORIZONTAL_SPLIT leftsplit component)
        allSplit (JSplitPane. JSplitPane/VERTICAL_SPLIT rightsplit debug-scroll)
        ]

    (.setResizeWeight leftsplit 0.25)
    (.setResizeWeight rightsplit 0.25)
    (.setResizeWeight allSplit 0.75)
    (.add root search-panel BorderLayout/NORTH)
    (.add root allSplit  BorderLayout/CENTER)

    (.setBorder search-panel (BorderFactory/createEmptyBorder 5 5 10 5))
    (.setBorder left-panel   (BorderFactory/createEmptyBorder 5 0 0 0))
    (.setBorder middle-panel (BorderFactory/createEmptyBorder 5 0 0 0))
    (.setBorder root (BorderFactory/createEmptyBorder 5 5 5 5))



    (.setFont left-list larger-font)
    (.setFont middle-list larger-font)
    (.setFont search-field larger-font) ; redundant, men tydligt

    (doto (.getDocument search-field)
      (.addDocumentListener
       (proxy [javax.swing.event.DocumentListener] []
         (insertUpdate [_]
           (let [text (.getText search-field)
                 ]
             (get-words text)
             #_(update-left words)))
         (removeUpdate [e] (.insertUpdate this e))
         (changedUpdate [_]))))

    (let [add-selected
          (fn []
            (when-let [val (.getSelectedValue left-list)]
              (swap! middle-set #(conj-unique % val))))]

      (.addMouseListener left-list
                         (proxy [MouseAdapter] []
                           (mouseClicked [e]
                             (when (= 1 (.getClickCount e))
                               (add-selected)))))
      (.addKeyListener left-list
                       (proxy [KeyAdapter] []
                         (keyPressed [e]
                           (when (#{\newline \space} (.getKeyChar e))
                             (add-selected))))))

    (let [remove-selected
          (fn []
            (when-let [val (.getSelectedValue middle-list)]
              (swap! middle-set #(remove-item % val))))]
      (.addMouseListener middle-list
                         (proxy [MouseAdapter] []
                           (mouseClicked [e]
                             (when (= 1 (.getClickCount e))
                               (remove-selected)))))
      (.addKeyListener middle-list
                       (proxy [KeyAdapter] []
                         (keyPressed [e]
                           (when (#{\newline \space} (.getKeyChar e))
                             (remove-selected))))))

    
    (doto frame
      (.setContentPane root)
      (.pack)
      (.addWindowListener (close-window frame))
      (.setDefaultCloseOperation JFrame/DO_NOTHING_ON_CLOSE)
      (.setVisible true))))


(defn main []
  (EDT! (create-ui))

  (book/book append-debug))



(defn new-ui []
  (EDT!
   (when @*frame
     (.dispose @*frame)) ; döda tidigare fönster snyggt
   
   (reset! *frame (create-ui))))

(comment 
  (new-ui)
)
