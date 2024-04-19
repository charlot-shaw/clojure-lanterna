(ns lanterna.screen
  (:import
   com.googlecode.lanterna.screen.Screen
   com.googlecode.lanterna.TerminalPosition
   com.googlecode.lanterna.SGR
   com.googlecode.lanterna.terminal.Terminal
   com.googlecode.lanterna.TextCharacter
   com.googlecode.lanterna.TextColor
   com.googlecode.lanterna.screen.TerminalScreen)
  (:require
   [lanterna.common :refer [parse-key block-on]]
   [lanterna.constants :as c]
   [lanterna.terminal :as t]))

(set! *warn-on-reflection* true)

(defn- enumerate [s]
  (map vector (iterate inc 0) s))

(defn ^Screen terminal-screen [^Terminal terminal]
  (TerminalScreen. terminal))

(defn add-resize-listener
  "Create a listener that will call the supplied fn when the screen is resized.

  The function must take two arguments: the new number of columns and the new
  number of rows.

  The listener itself will be returned.  You don't need to do anything with it,
  but you can use it to remove it later with remove-resize-listener.

  "
  [^TerminalScreen screen listener-fn]
  (t/add-resize-listener (.getTerminal screen)
                         listener-fn))

(defn remove-resize-listener
  "Remove a resize listener from the given screen."
  [^TerminalScreen screen listener]
  (t/remove-resize-listener (.getTerminal screen) listener))

(defn start
  "Start the screen.  Consider using in-screen instead.

  This must be called before you do anything else to the screen.

  "
  [^Screen screen]
  (.startScreen screen))

(defn stop
  "Stop the screen.  Consider using in-screen instead.

  This should be called when you're done with the screen.  Don't try to do
  anything else to it after stopping it.

  TODO: Figure out if it's safe to start, stop, and then restart a screen.

  "
  [^Screen screen]
  (.stopScreen screen))


(defmacro in-screen
  "Start the given screen, perform the body, and stop the screen afterward."
  [screen & body]
  `(let [screen# ~screen]
     (start screen#)
     (try ~@body
       (finally (stop screen#)))))


(defn get-size
  "Return the current size of the screen as [cols rows]."
  [^Screen screen]
  (let [size (.getTerminalSize screen)
        cols (.getColumns size)
        rows (.getRows size)]
    [cols rows]))


(defn redraw
  "Draw the screen.

  This flushes any changes you've made to the actual user-facing terminal.  You
  need to call this for any of your changes to actually show up.

  "
  [^Screen screen]
  (.refresh screen))


(defn move-cursor
  "Move the cursor to a specific location on the screen.

  This won't affect where text is printed when you use put-string -- the
  coordinates passed to put-string determine that.

  This is only used to move the cursor, presumably right before a redraw so it
  appears in a specific place.

  "
  ([^Screen screen x y]
   (.setCursorPosition screen (TerminalPosition. x y)))
  ([^Screen screen [x y]]
   (.setCursorPosition screen (TerminalPosition. x y))))

(defn get-cursor
  "Return the cursor position as [col row]."
  [^Screen screen]
  (let [pos (.getCursorPosition screen)
        col (.getColumn pos)
        row (.getRow pos)]
    [col row]))


(defn put-string
  "Put a string on the screen buffer, ready to be drawn at the next redraw.

  x and y are the column and row to start the string.
  s is the actual string to draw.

  Options can contain any of the following:

  :fg - Foreground color.
   This can be either a keyword or a string.
   If it's a keyword, it should be one of (keys lanterna.constants/colors).
   Not all terminals support all colormodes, you can consult lanterna's 
   [TextColor documentaion](https://mabe02.github.io/lanterna/apidocs/3.1/com/googlecode/lanterna/TextColor.html)
   to learn more.
   If it is a string, it must be in one of the following formats.

   * \"blue\", a string matching a color constant.
   * \"#1 \", a string consisting of an int between 0 and 255.
     This will use the indexed colormode, looking up the color in the terminal's own theme.
   * \"#a1a1a1\", a hex color code in a string. This draws using the color provided in RGB mode..

   As an escape hatch, directly providing a TextColor will use that to draw instead.
   an unparseable input will draw with (lanterna.constants/colors :default) or error.


  :bg - Background color.
  The same options for :fg, but will apply to the background instead.

  :styles - Styles to apply to the text.
  Can be a set containing some/none/all of (keys lanterna.constants/styles).
  (default #{})

  "
  ;; TODO: this function needs to be brought up to date with lanterna 3
  ([^Screen screen x y s] (put-string screen x y s {}))
  ([^Screen screen col row ^String s {:keys [fg bg styles]
                                      :or {fg :default
                                           bg :default
                                           styles #{}}}]
   (let [styles ^"[Lcom.googlecode.lanterna.SGR;"
         (into-array SGR (map c/styles styles))
         col (int col)
         row (int row)
         fg  (t/parse-color fg)
         bg  (t/parse-color bg)]
     (reduce (fn [acc ^java.lang.Character c]
               (let [col (:col acc)
                     row (:row acc)
                     tc (TextCharacter. c fg bg styles)]
                 (.setCharacter screen col row tc)
                 (update acc :col inc)))
             {:col col :row row}
             s)
     nil)))

(defn put-sheet
  "EXPERIMENTAL!  Turn back now!

  Draw a sheet to the screen (buffered, of course).

  A sheet is a two-dimentional sequence of things to print to the screen.  It
  will be printed with its upper-left corner at the given x and y coordinates.

  Sheets can take several forms.  The simplest sheet is a vector of strings:

    (put-sheet scr 2 0 [\"foo\" \"bar\" \"hello!\"])

  This would print something like

     0123456789
    0  foo
    1  bar
    2  hello!

  As you can see, the rows of a sheet do not need to all be the same size.
  Shorter rows will *not* be padded in any way.

  Rows can also be sequences themselves, of characters or strings:

    (put-sheet scr 5 0 [[\\s \\p \\a \\m] [\"e\" \"g\" \"g\" \"s\"]])

     0123456789
    0     spam
    1     eggs

  Finally, instead of single characters of strings, you can pass a vector of a
  [char-or-string options-map], like so:

    (put-sheet scr 1 0 [[[\\r {:fg :red}] [\\g {:fg :green}]]
                        [[\\b {:fg :blue}]]])

     0123456789
    0 rg
    1 b

  And the letters would be colored appropriately.

  Finally, you can mix and match any and all of these within a single sheet or
  row:

    (put-sheet scr 2 0 [\"foo\"
                        [\"b\" \\a [\\r {:bg :yellow :fg :black}])

  "
  [screen x y sheet]
  (letfn [(put-item [c r item]
            (cond
              (string? item) (put-string screen c r item)
              (char? item)   (put-string screen c r (str item))
              (vector? item) (let [[i opts] item]
                               (if (char? i)
                                 (put-string screen c r (str i) opts)
                                 (put-string screen c r i opts)))
              :else nil ; TODO: die loudly
              ))
          (put-row [r row]
            (doseq [[c item] (enumerate row)]
              (put-item (+ x c) r item)))]
    (doseq [[i row] (enumerate sheet)]
      (if (string? row)
        (put-string screen x (+ y i) row)
        (put-row (+ y i) row)))))

(defn clear
  "Clear the screen.

  Note that this is buffered like everything else, so you'll need to redraw
  the screen to see the effect.

  "
  [^Screen screen]
  (.clear screen))


(defn get-key
  "Get the next keypress from the user, or nil if none are buffered.

  If the user has pressed a key, that key will be returned (and popped off the
  buffer of input).

  If the user has *not* pressed a key, nil will be returned immediately.  If you
  want to wait for user input, use get-key-blocking instead.

  "
  [^Screen screen]
  (parse-key (.pollInput screen)))

(defn get-key-blocking
  "Get the next keypress from the user.

  If the user has pressed a key, that key will be returned (and popped off the
  buffer of input).

  If the user has *not* pressed a key, this function will block, checking every
  50ms.  If you want to return nil immediately, use get-key instead.

  Options can include any of the following keys:

  :interval - sets the interval between checks
  :timeout  - sets the maximum amount of time blocking will occur before
              returning nil

  "
  ([^Screen screen] (get-key-blocking screen {}))
  ([^Screen screen {:keys [interval timeout] :as opts}]
     (block-on get-key [screen] opts)))