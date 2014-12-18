(import hybot)
(import [twisted.internet [reactor]])
(import colorama)
(import [userpass])

(defmain [&rest args]
  (let [[chan (get args 1)]]
    (.init colorama)
    (print (.format "Channel: #{0}" chan))
    (.connectTCP 
        reactor 
        (str "irc.twitch.tv") 
        6667 
        (.BetBotFactory hybot 
            (str (+ "#" chan)) 
            userpass.nickname 
            userpass.password))
    (.run reactor)))