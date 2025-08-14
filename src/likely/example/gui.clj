(ns likely.example.gui
  (:require [clojure.string]
            [clojure.core.async :as a]
            [likely.example.book :as book]
            [likely.example.debug :as debug]
            [likely.example.eventdispatchthread :refer [EDT!]])
  (:import [javax.swing JFrame JPanel JTextField JTextArea
            JList JScrollPane ListSelectionModel ListCellRenderer
            BorderFactory DefaultListModel JSplitPane JLabel]
           [java.awt BorderLayout Dimension Color Font]
           [javax.swing JFrame]
           [java.awt.event WindowAdapter]
           [java.awt.event MouseAdapter KeyAdapter]))

;; An atom for the main frame that can be created and closed. For repl driven usage 
(defonce ^:private *frame (atom nil))

;; Close and clear the frame and when there is *frame, otherwise do System exit
(defn  close-window [frame]
  (proxy [WindowAdapter] []
    (windowClosing [_]
      ;; return to the console logger
      (debug/install-console-logger!)
      (if @*frame
        (do
          (println "Disposar fönster. Jag tror jag är (REPL)...")
          (.dispose frame)
          (reset! *frame nil))
        (do
          (println "Avslutar JVM (main)...")
          (System/exit 0))))))


;; A place for our search commands. This should hold a sliding buffer of 1 which is the latest next search for the worker thread.
(defonce ^:private search-ch* (atom nil))

;; Starts a daemon worker using that reads jobs from a sliding buffer. 
;; That is only that last published work will be done. Earlier are considered old. 
;; apply-results! takes the result of work, while work is the function doing work.
;; Worker stops when search-ch* is closed
(defn ensure-search-worker!
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

;; Closes and resets the *search-ch, and hence the worker
(defn stop-search-worker!
  []
  (when-let [ch @search-ch*]
    (a/close! ch)
    (reset! search-ch* nil)))

(defn request-search!
  "Kallas från din DocumentListener: skicka in nuvarande text."
  [q apply-results! get-words]
  (ensure-search-worker! apply-results! get-words)
  (a/put! @search-ch* q))     ;; put! är icke-blockerande på GUI-tråden







;; *************** important components

(def search-field (JTextField.))
(def default-font (.getFont search-field))
(def debug-area (doto (JTextArea.)
                  (.setEditable false)
                  (.setLineWrap true)
                  (.setWrapStyleWord true)))



;; ***************** The zebra list for documents

(defn multiline-renderer []
  (proxy [javax.swing.JTextArea ListCellRenderer] []
    (getListCellRendererComponent
      [list value index isSelected cellHasFocus]
      (let [area (JTextArea. ^String value)
            width (.getWidth list)
            real-width (if (pos? width) (- width 20) 400)]
        
        (.setLineWrap area true)
        (.setWrapStyleWord area true)
        (.setEditable area false)
        (.setOpaque area true)
        (.setFont area default-font)
        (.setBorder area (BorderFactory/createEmptyBorder 2 4 2 4))
        (.setSize area (Dimension. real-width Integer/MAX_VALUE))
        (.setPreferredSize area (.getPreferredSize area))
        
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
    {:scrollpane (JScrollPane. jlist)
     :jlist jlist}))


;; ********* debug logging

(defn append-debug [^String text]
  (.append debug-area (str text "\n"))
  (.setCaretPosition debug-area (.getLength (.getDocument debug-area))))

(defmacro with-stdout
  "Catches stdout into a string while doing body, returns [value string]. 
   Pretty much like with-out-str but provides the value as well, and not just the string"
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       (let [a# (do ~@body)]
         [a# (str s#)]))))


(def select-list-model (DefaultListModel.))

(def chosen-list-model (DefaultListModel.))

(def found-list-model (DefaultListModel.))


(defn paragraphs-matching [words]
  (->> (mapcat book/refs words)
       (book/sort-paragraphs-on-words words)
       (map #(str (:book %) " - " (:chapter %) ":\n" (:text %)))))

;; An atom holding selected words, with a watch that
;; updates the selected list accordingly
(def selected-words
  (let [words-a (atom [])]
    (add-watch
     words-a "key"
     (fn [_key _atom _old-state new-state]
       (EDT!
         (let [paragraphs (paragraphs-matching new-state)] 
           (.clear chosen-list-model)
           (doseq [w new-state]
             (.addElement chosen-list-model w))
           (.clear found-list-model)
           (doseq [w paragraphs]
             (.addElement found-list-model w))))))
    words-a))

(defn conj-unique
  "conj xs x, unless x already is in xs. A vector becomes a cheap oredered set"
  [xs x]
  (if (some #(= % x) xs)
    xs
    (conj xs x)))

(defn remove-item [v item]
  (vec (remove #(= % item) v)))




(defn update-left [words]
  (EDT!
   (.clear select-list-model)
   (doseq [w words]
     (.addElement select-list-model w))))

(defn get-words [text]
  (request-search! text
                   (fn [[result debug-text]]
                     (debug/debug! debug-text)
                     (update-left result))
                   (fn [text] (with-stdout
                                (book/search text)))))

(defn search-listener-on [search-field]  
  (.addDocumentListener
   (.getDocument search-field)
   (proxy [javax.swing.event.DocumentListener] []
     (insertUpdate [_]
       (get-words (.getText search-field)))
     (removeUpdate [e] (.insertUpdate this e))
     (changedUpdate [_]))))


(defn add-selected-listeners [select-list selected-words]
   (let [add-selected
          (fn []
            (when-let [val (.getSelectedValue select-list)]
              (swap! selected-words #(conj-unique % val))))]

      (.addMouseListener select-list
                         (proxy [MouseAdapter] []
                           (mouseClicked [e]
                             (when (= 1 (.getClickCount e))
                               (add-selected)))))
      (.addKeyListener select-list
                       (proxy [KeyAdapter] []
                         (keyPressed [e]
                           (when (#{\newline \space} (.getKeyChar e))
                             (add-selected)))))))

(defn add-chosen-listeners [chosen-list selected-words]
  (let [remove-selected
        (fn []
          (when-let [val (.getSelectedValue chosen-list)]
            (swap! selected-words #(remove-item % val))))]
    (.addMouseListener chosen-list
                       (proxy [MouseAdapter] []
                         (mouseClicked [e]
                           (when (= 1 (.getClickCount e))
                             (remove-selected)))))
    (.addKeyListener chosen-list
                     (proxy [KeyAdapter] []
                       (keyPressed [e]
                         (when (#{\newline \space} (.getKeyChar e))
                           (remove-selected)))))))

(defn make-split-pane [select-words-panel chosen-panel document-list debug-panel]
  (let [left-split (JSplitPane. JSplitPane/HORIZONTAL_SPLIT
                                select-words-panel
                                chosen-panel)
      right-split (JSplitPane. JSplitPane/HORIZONTAL_SPLIT
                               left-split
                               document-list)
      
      split-pane (JSplitPane. JSplitPane/VERTICAL_SPLIT
                              right-split
                              debug-panel)]
  (.setResizeWeight left-split 0.25)
  (.setResizeWeight right-split 0.25)
  (.setResizeWeight split-pane 0.75)
  split-pane))

(defn create-ui []
  (let [frame (JFrame. "Clever search in books")
        root (doto (JPanel. (BorderLayout.))
               (.setPreferredSize (Dimension. 800 600)))
        document-list (:scrollpane (make-zebra-jlist found-list-model))
        debug-panel (JScrollPane. debug-area)
        select-list (doto (JList. select-list-model)
                      (.setSelectionMode ListSelectionModel/MULTIPLE_INTERVAL_SELECTION))
        select-words-panel (doto (JPanel. (BorderLayout.))
                             (.add  (JLabel. "Select words:")
                                    BorderLayout/NORTH)
                             (.add (doto (JScrollPane. select-list)
                                     (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED))
                                   BorderLayout/CENTER))
        chosen-list (JList. chosen-list-model)
        chosen-panel (doto (JPanel. (BorderLayout.))
                       (.add  (javax.swing.JLabel. "Selected words:") BorderLayout/NORTH)
                       (.add  (JScrollPane. chosen-list) BorderLayout/CENTER))
        split-pane (make-split-pane select-words-panel chosen-panel document-list debug-panel)
        
        
        search-panel (doto (JPanel. (BorderLayout.))
                       (.add (javax.swing.JLabel. "Query: ") BorderLayout/WEST)
                       (.add search-field BorderLayout/CENTER))
        larger-font (Font. (.getName default-font)
                           Font/PLAIN
                           (+ 4 (.getSize default-font)))]

    (debug/install-gui-logger! append-debug)
    (.add root search-panel BorderLayout/NORTH)
    (.add root split-pane  BorderLayout/CENTER)

    (.setBorder search-panel (BorderFactory/createEmptyBorder 5 5 10 5))
    (.setBorder select-words-panel   (BorderFactory/createEmptyBorder 5 0 0 0))
    (.setBorder chosen-panel (BorderFactory/createEmptyBorder 5 0 0 0))
    (.setBorder root (BorderFactory/createEmptyBorder 5 5 5 5))

    (.setFont select-list larger-font)
    (.setFont chosen-list larger-font)
    (.setFont search-field larger-font) ; redundant, men tydligt

    (search-listener-on search-field)
    (add-selected-listeners select-list selected-words)
    (add-chosen-listeners chosen-list selected-words)

    (doto frame
      (.setContentPane root)
      (.pack)
      (.addWindowListener (close-window frame))
      (.setDefaultCloseOperation JFrame/DO_NOTHING_ON_CLOSE)
      (.setVisible true))))


(defn main []
  (create-ui)
  (book/book))

(defn new-ui []
  (EDT!
   (when @*frame
     (.dispose @*frame))    
   (reset! *frame (create-ui))))

(comment 
  (new-ui)
)
