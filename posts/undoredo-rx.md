---
title: Elegant undo/redo recording with Reactive Extensions
published: 2014-11-11
---

I've been spending a lot of time lately thinking about functional reactive programming. Since I'm currently primarily a .NET developer, the obvious library for this kind of thing is Reactive Extensions (Rx), which provides a lot of convenient methods (often called combinators) for working with IObservables and IObservers. While not quite as expressive as other FRP libraries (I'm looking at you, [reactive-banana](https://www.haskell.org/haskellwiki/Reactive-banana)), it's the best option for doing this sort of thing on .NET.

To me, the holy grail of FRP is to implement a large-scale application using as little mutable state as possible. Rx goes a long way in making this possible, but there are several things which are still tricky to model using these tools. Because it's a relatively new library, which is part of a relatively new paradigm (as far as mainstream adoption goes), there also isn't a lot of information online about implementing more complicated functionality using only the concepts provided by this framework.

The first difficulty I discovered was implementing undo and redo for an application developed with Rx. Specifically, how do you take several different observable sequences, record their values in a central place (the undo/redo stacks), and then at a later point when an undo or redo operation is invoked, replay those values in their original sequences? Is this even the correct way to approach the problem of undo and redo in Rx? Searching online yielded next to no information on the subject, and not just in Rx, but FRP in general. In my search, I found [a compelling example in Haskell that demonstrated how to define mutually recursive reactive circuits](https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana-wx/src/CRUD.hs), which would allow you to define a reactive value in terms of itself. This would allow me to combine the output of a stream with some form of undo/redo trigger, and then feed the result back into itself. Unfortunately, this kind of thing proved to be absurdly difficult to implement for Rx, because Haskell is lazy, [.NET has shoddy tail-call optimization](http://stackoverflow.com/questions/491376/why-doesnt-net-c-optimize-for-tail-call-recursion), and the library used in the example is [a completely different kind of reactive framework](https://blogs.janestreet.com/breaking-down-frp/) (Rx is monadic, reactive-banana is applicative).

So with minimal help from the internet, I set out to implement some kind of reactive undo/redo recorder that satisfies my original design goal: in the definition of an observable sequence, you can specify a point at which values are recorded for undo and redo. If the recorded value is then re-played via an undo or redo action, then that value is re-inserted into the observable sequence at the exact same point it was recorded.

Here is the result: <https://gist.github.com/Steell/e3c57b88abe2a708e526>

The source code is rather short and is heavily documented, so if you really want to understand how it works it shouldn't be too difficult. At a high level, the actual undo and redo stacks are stored in an immutable structure. Since this structure changes over time, I represented its various states as an observable sequence. The state can be mutated in three ways: recording a new action, performing an undo, and performing a redo. When performing an undo or redo, not only is the stack updated, but the operation that was originally recorded (or its inverse) is performed. When recording data from a source stream, it is packaged in such a way that when an undo or redo occurs, that data is sent back into the stream via an Rx Subject.

Below are some samples demonstrating some of the various ways you can use the ReactiveUndoRedoRecorder class.

<script src="https://gist.github.com/Steell/145785ba00289e18e799.js" />
