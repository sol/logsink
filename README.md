# logsink

`logsink` is meant to be used in conjunction with
[`logging-facade`](http://hackage.haskell.org/package/logging-facade). To log
messages application code uses functions from `System.Logging.Facade`.
`logsink` itself is a backend for `logging-facade` and allows to configure
logging behavior.

`logsink` has built-in support for `stderr` and `syslog`. The following example
shows how to direct log messages to `syslog`:

```haskell
import qualified System.Logging.Facade as Log
import           System.Logging.LogSink.Config

main :: IO ()
main = do
  setupLogging [defaultSinkConfig { sinkConfigTarget = SysLog }]
  Log.error "This is an error message."
```

You can also configure the message format. (See documentation of `System.Logging.LogSink.Format` for allowed format directives.)

```haskell
main2 :: IO ()
main2 = do
  setupLogging [defaultSinkConfig { sinkConfigFormat = "{level} - {timestamp}: {message}" }]
  Log.error "This is an error message."
```
