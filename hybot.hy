(import [twisted.words.protocols [irc]])
(import [twisted.internet [protocol error reactor]])

(import re random)
(import [colorama [Fore]])

(defn color-string [string color]
  (.format (+ "{0}" string "{1}") color Fore.RESET))

(defclass HighGambler [object]
  [[--init--
    (fn [self]
      (setv self.coins 0)
      (setv self.scores (list-comp 0 [x (range 8)]))
      None)]

   [process-message
    (fn [self client user msg]
      (if (or (in "stockbets" user)
              (in "amiibofighter" user))
        (do 
          (print (.format "Got stock/amii message: {0}" msg))
          (cond 
            [(in "Betting open for" msg) (.place-bet self client.say-message)]
            [(in "!gamble winner" msg) (.track-winner self msg client.say-message)]
            [(in "coins:" msg) (.track-coins self msg)]))))]

   [place-bet
    (fn [self say-fn]
      (let [[reduce-fn (fn [acc x]
                          (let [[ai (get acc 0)] 
                                [av (get acc 1)]
                                [i (get x 0)] 
                                [v (get x 1)]]
                            (cond [(> v av) [i v]]
                                  [(and (= v av) (>= (.random random) 0.5)) [i v]]
                                  [True acc])))] 
            [maxi (reduce reduce-fn (enumerate self.scores) [0 0])]
            [bet-index (+ (get maxi 0) 1)]
            [bet-amt (% (int (/ self.coins (.randint random 16 25))) 10)]
            [nice-bet-amt (int (- bet-amt (% bet-amt 10)))]
            [delay (+ 6 (* 8 (.random random)))]
            [msg (.format "!bet {0} {1}" nice-bet-amt bet-index)]]
        (unless (<= bet-amt 5) (say-fn msg delay))))]

   [track-winner
    (fn [self msg say-fn]
      (let [[win-match (.search re r"(\d)$" msg)]
            [coin-msg "!coins"]
            [delay (+ 5 (* 4 (.random random)))]]
        (unless (= win-match None)
          (let [[test (print win-match (.groups win-match))]
                [winner (int (.group win-match (int 1)))]
                [winner-index (- winner 1)]
                [message (color-string 
                           (.format "{0} won the round: {1}" winner self.scores) 
                           Fore.GREEN)]
                [new-score (+ (get self.scores winner-index) 1)]]
            (assoc self.scores winner-index new-score)))
        (say-fn coin-msg delay)))]

    [track-coins
     (fn [self msg]
       (let [[coin-match (.search re r"Nopefully \- (\d+)" msg)]]
         (unless (= coin-match None)
           (let [[test (print (.groups coin-match))]
                 [coin (int (.group coin-match (int 1)))]]
             (print (color-string 
                      (.format "Got coins: {0}" coin) 
                      Fore.BLUE))
             (setv self.coins coin)))))]])

(defclass BetBot [irc.IRCClient]
  [[-get-nickname (fn [self] self.factory.nickname)]
   [-get-password (fn [self] self.factory.password)]

   [--init--
    (fn [self]
      (setv self.gambler (HighGambler))
      (setv BetBot.nickname (property BetBot.-get-nickname))
      (setv BetBot.password (property BetBot.-get-password))
      None)]

   [signedOn
    (fn [self]
      (.join self self.factory.channel)
      (print (.format "Signed on as {0}" self.nickname)))]

   [joined
    (fn [self channel] (print (.format "Joined {0}" channel)))]

   [privmsg
    (fn [self user channel msg] (.process-message self.gambler self user msg))]

   [say-message
    (fn [self message delay]
      (print (color-string (.format "Saying on {0}: {1}" self.factory.channel message) Fore.GREEN))
      (.callLater reactor delay self.say self.factory.channel (str message)))]])

(defclass BetBotFactory [protocol.ReconnectingClientFactory]
  [[--init--
    (fn [self channel nickname password]
      (setv BetBotFactory.protocol BetBot)
      (setv self.channel channel)
      (setv self.nickname nickname)
      (setv self.password password)
      None)]

   [startedConnection
    (fn [self connector] (print (.format "Connecting! ({0})" connector)))]

   [clientConnectionLost
    (fn [self connector reason]
      (print (.format "Lost connection ({0}), reconnecting..." connector))
      (unless (.check reason error.ConectionDone)
        (.connect connector)))]

   [clientConnectionLost
    (fn [self connector reason] (print (.format "Could not connect: ({0})" reason)))]])