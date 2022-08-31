# Java FutureTask and Singleton

FutureTask and Singleton are a pair of Java classes that I wrote to refine some Java coding techniques.

## FutureTask

It is a common situation to need to do some work in the background while a foreground thread continues to deliver interactivity to the user.

FutureTask is a standard Java library class for handling tasks that complete in the future. I worked with this library feature and wrote a Class worker queue implementation that allowed for objects to be instantiated and automatically get added to the worker queue for completion.

## Singleton

Singleton is an example of the Singleton pattern. This pattern is notoriously difficult too implement in multi-threaded code. This is my implementation based on research and development.
