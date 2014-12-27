from twisted.words.protocols import irc
from twisted.internet import reactor, protocol
import random, sys, threading, config

# This program parses a twitch.tv chat to a database and generates sample
# messages using Markov chain with k = 1.
# How to use:
# 1. Set 'username' variable in config.py to your twitch.tv username.
# 2. Set 'password' variable in config.py to your OAuth token generated using
# http://www.twitchapps.com/tmi/
# 3. Run as "python2 twitcher.py channelname" choosing a channel with an
# active chat.
# 4. Press Enter to generate a new message, Ctrl+C to quit.

db = {}

def log(msg):
    # change to True to enable verbose mode
    if False: print msg

def updateDb(w1, w2):
    db[w1] = db.get(w1, []) + [w2]

class LogBot(irc.IRCClient):
    def __init__(self):
        # don't forget to set them up
        self.nickname = config.nickname
        self.password = config.password

    def connectionMade(self):
        irc.IRCClient.connectionMade(self)
        log('[connected]')

    def connectionLost(self, reason):
        irc.IRCClient.connectionLost(self, reason)
        log('[disconnected]')

    def signedOn(self):
        self.join(self.factory.channel)

    def joined(self, channel):
        log("[joined %s]" % channel)

    def privmsg(self, user, channel, msg):
        log("<%s> %s" % (user.split('!', 1)[0], msg))
        words = msg.split()
        updateDb(0, words[0])
        updateDb(words[-1], 1)
        for i in range(1, len(words)): updateDb(words[i - 1], words[i])

    def action(self, user, channel, msg):
        log("* %s %s" % (user.split('!', 1)[0], msg))

    def irc_NICK(self, prefix, params):
        log("%s is now known as %s" % (prefix.split('!')[0], params[0]))

class LogBotFactory(protocol.ClientFactory):
    def __init__(self, channel):
        self.channel = channel

    def buildProtocol(self, addr):
        p = LogBot()
        p.factory = self
        return p

    def clientConnectionLost(self, connector, reason):
        connector.connect()

    def clientConnectionFailed(self, connector, reason):
        log("connection failed: " + reason)
        reactor.stop()

class Builder(threading.Thread):
    def run(self):
        f = LogBotFactory(sys.argv[1])
        reactor.connectTCP("irc.twitch.tv", 6667, f)
        reactor.run(installSignalHandlers=0)

if __name__ == '__main__':
    builder = Builder()
    builder.daemon = True
    builder.start()
    while True:
        try:
            raw_input()
            words = [random.choice(db[0])]
            while True:
                newWord = random.choice(db[words[-1]])
                if newWord == 1: break
                words.append(newWord)
            print ' '.join(words)
        except KeyboardInterrupt:
            sys.exit()
