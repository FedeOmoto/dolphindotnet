# Generating assembly wrappers #

You've your own .NET assembly and want to generate wrappers for all the classes that it contains, or you just want to generate wrappers for some of the standard .NET framework assemblies.

In the following example we're going to generate wrappers for all the classes contained in the mscorlib assembly. It applies also to any assembly you want to generate wrappers for, just replace the assembly name. It's important to note that the assembly name must NOT include its filename extension (exclude the .exe/.dll extension from it).

```
assembly := MonoAssembly named: 'mscorlib'.
assembly generateWrappers.
```

If you've evaluated this example as is, be warned that it'll take a while to generate all the class wrappers (1753 classes and 39575 methods).

Lets play a little with the [MessageBox](http://msdn.microsoft.com/en-us/library/system.windows.forms.messagebox.aspx) class of the System.Windows.Forms assembly. First, generate wrappers for the assembly:

```
assembly := MonoAssembly named: 'System.Windows.Forms'.
assembly generateWrappers.
```

Again, this will take some time...

Now, some code from the Microsoft's [MessageBox](http://msdn.microsoft.com/en-us/library/system.windows.forms.messagebox.aspx) class Examples section:

```
yes := System_Windows_Forms_DialogResult Yes.
no := System_Windows_Forms_DialogResult No.
buttons := System_Windows_Forms_MessageBoxButtons YesNo.
icon := System_Windows_Forms_MessageBoxIcon Question.
result := System_Windows_Forms_MessageBox 
			Show: 'Are you sure that you would like to close the form?'
			caption: 'Form Closing'
			buttons: buttons
			icon: icon.
Transcript
	show: 'Closing form: ' , (result = yes) displayString;
	cr
```