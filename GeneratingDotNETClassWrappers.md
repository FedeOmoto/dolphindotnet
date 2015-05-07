# Generating .NET class wrappers #

So, you've installed Mono, the DolphinDotNET package, configured/initialized the Mono runtime and now you're wondering "How do I use all that .NET classes?". If you want to use some .NET classes all you've to do is generating wrappers for them.

Let suppose that you want to use the [DateTime](http://msdn.microsoft.com/en-us/library/system.datetime.aspx) class from the mscorlib assembly, in order to generate the wrapper you can evaluate:

```
assembly := MonoAssembly named: 'mscorlib'.
dateTimeMonoClass := assembly class: 'DateTime' namespace: 'System'.
(MonoClassBuilder on: dateTimeMonoClass) build.
```

The MonoClassBuilder class works on MonoClass objects and generate wrappers for them on #build.
Now you can take a look at the generated wrapper, it's a subclass of MonoObject named System\_DateTime.

Lets play with the generated wrapper:

```
dateTime1 := System_DateTime new.
dateTime1 ToString. "Display It: '1/1/0001 12:00:00 AM'"

dateTime2 := System_DateTime DateTime: 1979 month: 10 day: 26.
dateTime2 ToString. "Display It: '10/26/1979 12:00:00 AM'"
dateTime2 GetDateTimeFormats. "Inspect It"

System_DateTime Today ToString. "Display It"
System_DateTime Now ToString. "Display It"
```