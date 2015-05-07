# Initializing the Mono runtime #

Once you've installed Mono and the DolphinDotNET package, the next step before starting using .NET from Dolphin Smalltalk is configuring/initializing the Mono runtime.

Assuming you've installed Mono in the "C:\Mono-2.8" directory, you can evaluate this in a Workspace to do the runtime initialization:

```
| monoRuntime |

monoRuntime := MonoRuntime current.
monoRuntime assemblyPath: 'C:\Mono-2.8\lib'.
monoRuntime configurationPath: 'C:\Mono-2.8\etc'.
monoRuntime initialize.
```

And that's it! Now you can start using .NET classes from Dolphin.

Although, there are some things you should know about the Mono runtime initialization process:

  * The default .NET framework version is 4.0, if for some reason you want to use the 2.0 version you can do it by calling #version: prior invoking #initialize.
  * Some assemblies require to be executed in a specific application domain name, you can set it with #domainName: prior invoking #initialize.
  * Your assemblies (that is, the assemblies that are not part of the .NET framework) are searched in various default directories, see MonoRuntime>>setMonoPath. If you want to place them in some other directories you can use #additionalAssemblyPaths:

So, taking all of this into account, the previous Workspace code snippet can be written as:

```
| monoRuntime |

monoRuntime := MonoRuntime current.
monoRuntime version: 4.
monoRuntime domainName: 'MyAssemblyName'.
monoRuntime assemblyPath: 'C:\Mono-2.8\lib'.
monoRuntime configurationPath: 'C:\Mono-2.8\etc'.
monoRuntime additionalAssemblyPaths: 'C:\my\own\assemblies'.
monoRuntime initialize.
```