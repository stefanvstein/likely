(ns likely.example.gui
  (:require [clojure.string]
            [likely.example.book :as book])
  (:import [javax.swing JFrame JPanel JTextField JTextArea
            JList JScrollPane ListSelectionModel ListCellRenderer
            BorderFactory SwingUtilities DefaultListModel JSplitPane]
           [java.awt BorderLayout Dimension Color Font]
           [javax.swing JFrame]
           [java.awt.event WindowAdapter]
           [java.awt.event MouseAdapter KeyAdapter]))

(defonce ^:private *frame (atom nil))
(import '[java.awt Font])

(def debug-area (doto (JTextArea.)
                  (.setEditable false)
                  (.setLineWrap true)
                  (.setWrapStyleWord true)))

(defn append-debug
  [^String text]
  (SwingUtilities/invokeLater #((.append debug-area (str text "\n"))
                                (.setCaretPosition debug-area (.getLenght (.getDocument debug-area))))))

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
       (SwingUtilities/invokeLater
        #(let [paragraphs (get-paragraphs new-state)] ;; Det här ska nog ske i en agent som updaterar listan efter sökning är klar
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
  (.clear left-list-model)
  (doseq [w words]
    (.addElement left-list-model w)))

;(def current-search (atom nil))

(defn get-words [text]
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
               (append-debug s)
               (update-left r))))
  )




(defn create-ui []
  (let [frame (JFrame. "Sök för i helvete")
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


(defn -main []
  (SwingUtilities/invokeLater create-ui))

(defn- create-ui-on-edt []
  (when @*frame
    (.dispose @*frame)) ; döda tidigare fönster snyggt

  (reset! *frame (create-ui)))

(defn new-ui []
  (SwingUtilities/invokeLater
    ^Runnable #(create-ui-on-edt)))

(comment 
  (new-ui)
)
