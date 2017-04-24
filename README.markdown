# Elm Beyond the Basics - knowthen course

My version after taking the [Elm beyond the basics](http://courses.knowthen.com/courses/elm-beyond-the-basics)

## Next step

### Add support for pausing realtime updates

1. Set active to True on init
2. If "Pause realtime updates" are clicked
    2.1 Change active to False and run Command "stop listening"
    2.2 Unsubscribe to Tick
3. If "Restore realtime updates" are clicked
    3.1 Change active to True and run Command "start listening"
    3.2 Subscribe to Tick