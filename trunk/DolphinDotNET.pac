| package |
package := Package name: 'DolphinDotNET'.
package paxVersion: 1;
	basicComment: 'Copyright (c) 2010, Federico Omoto
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.

    * Neither the name of the author nor the names of its contributors may
      be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
'.


package classNames
	add: #BOOLEAN;
	add: #MonoArray;
	add: #MonoAssembly;
	add: #MonoAssemblyName;
	add: #MonoClass;
	add: #MonoClassBuilder;
	add: #MonoClassField;
	add: #MonoDomain;
	add: #MonoImage;
	add: #MonoLibrary;
	add: #MonoMemoryManager;
	add: #MonoMethod;
	add: #MonoMethodDesc;
	add: #MonoMethodSignature;
	add: #MonoObject;
	add: #MonoProperty;
	add: #MonoRuntime;
	add: #MonoString;
	add: #MonoType;
	add: #MonoVTable;
	yourself.

package methodNames
	add: #Array -> #asMonoArray;
	add: #ByteArray -> #asMonoArray;
	add: #Object -> #isMonoObject;
	add: #String -> #asMonoString;
	add: #UnicodeString -> #asMonoString;
	add: 'File class' -> #unixPathOf:;
	add: 'Object class' -> #isMonoObject;
	yourself.

package globalNames
	add: #MonoConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\ActiveX\Automation\ActiveX Automation';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	add: '..\..\IDB\IDB Method History';
	yourself).

package!

"Class Definitions"!

Object subclass: #MonoClassBuilder
	instanceVariableNames: 'monoClass class'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MonoMemoryManager
	instanceVariableNames: ''
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MonoRuntime
	instanceVariableNames: 'monoLibrary domain domainName version configurationFile assemblyPath configurationPath assemblyPreloadHookCallback'
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #MonoLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SBYTE subclass: #BOOLEAN
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VOID subclass: #MonoAssembly
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VOID subclass: #MonoAssemblyName
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'MonoConstants'
	classInstanceVariableNames: ''!
VOID subclass: #MonoClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'MonoConstants'
	classInstanceVariableNames: ''!
VOID subclass: #MonoClassField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'MonoConstants'
	classInstanceVariableNames: ''!
VOID subclass: #MonoDomain
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'monoLibrary'!
VOID subclass: #MonoImage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'MonoConstants'
	classInstanceVariableNames: ''!
VOID subclass: #MonoMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'MonoConstants'
	classInstanceVariableNames: ''!
VOID subclass: #MonoMethodDesc
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VOID subclass: #MonoMethodSignature
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VOID subclass: #MonoObject
	instanceVariableNames: 'monoLibrary gcHandle'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'monoClass'!
VOID subclass: #MonoProperty
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VOID subclass: #MonoType
	instanceVariableNames: ''
	classVariableNames: 'TypeClasses'
	poolDictionaries: 'MonoConstants'
	classInstanceVariableNames: ''!
VOID subclass: #MonoVTable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MonoObject subclass: #MonoArray
	instanceVariableNames: 'elementClass'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MonoObject subclass: #MonoString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Array methodsFor!

asMonoArray
	"Answer the receiver as a <MonoArray>."

	^MonoArray fromArray: self! !
!Array categoriesFor: #asMonoArray!converting!public! !

!ByteArray methodsFor!

asMonoArray
	"Answer the receiver as a <MonoArray>."

	^MonoArray fromByteArray: self! !
!ByteArray categoriesFor: #asMonoArray!converting!public! !

!File class methodsFor!

unixPathOf: aPathnameString 
	"Answers the Unix path of aPathnameString"

	^aPathnameString copyReplacing: $\ withObject: $/! !
!File class categoriesFor: #unixPathOf:!filename manipulation!public! !

!Object methodsFor!

isMonoObject
	"Answer whether the receiver is a <MonoObject>."

	^self class isMonoObject! !
!Object categoriesFor: #isMonoObject!public!testing! !

!Object class methodsFor!

isMonoObject
	"Answer whether the receiver is a <MonoObject>."

	^false! !
!Object class categoriesFor: #isMonoObject!public!testing! !

!String methodsFor!

asMonoString
	"Answer the receiver as a <MonoString>."

	^MonoString fromString: self! !
!String categoriesFor: #asMonoString!converting!public! !

!UnicodeString methodsFor!

asMonoString
	"Answer the receiver as a <MonoString>."

	^MonoString fromUnicodeString: self! !
!UnicodeString categoriesFor: #asMonoString!converting!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #MonoConstants put: (PoolConstantsDictionary named: #MonoConstants)!
MonoConstants at: 'MONO_FIELD_ATTR_ASSEMBLY' put: 16r3!
MonoConstants at: 'MONO_FIELD_ATTR_COMPILER_CONTROLLED' put: 16r0!
MonoConstants at: 'MONO_FIELD_ATTR_FAM_AND_ASSEM' put: 16r2!
MonoConstants at: 'MONO_FIELD_ATTR_FAM_OR_ASSEM' put: 16r5!
MonoConstants at: 'MONO_FIELD_ATTR_FAMILY' put: 16r4!
MonoConstants at: 'MONO_FIELD_ATTR_FIELD_ACCESS_MASK' put: 16r7!
MonoConstants at: 'MONO_FIELD_ATTR_HAS_DEFAULT' put: 16r8000!
MonoConstants at: 'MONO_FIELD_ATTR_HAS_MARSHAL' put: 16r1000!
MonoConstants at: 'MONO_FIELD_ATTR_HAS_RVA' put: 16r100!
MonoConstants at: 'MONO_FIELD_ATTR_INIT_ONLY' put: 16r20!
MonoConstants at: 'MONO_FIELD_ATTR_LITERAL' put: 16r40!
MonoConstants at: 'MONO_FIELD_ATTR_NOT_SERIALIZED' put: 16r80!
MonoConstants at: 'MONO_FIELD_ATTR_PINVOKE_IMPL' put: 16r2000!
MonoConstants at: 'MONO_FIELD_ATTR_PRIVATE' put: 16r1!
MonoConstants at: 'MONO_FIELD_ATTR_PUBLIC' put: 16r6!
MonoConstants at: 'MONO_FIELD_ATTR_RESERVED_MASK' put: 16r9500!
MonoConstants at: 'MONO_FIELD_ATTR_RT_SPECIAL_NAME' put: 16r400!
MonoConstants at: 'MONO_FIELD_ATTR_SPECIAL_NAME' put: 16r200!
MonoConstants at: 'MONO_FIELD_ATTR_STATIC' put: 16r10!
MonoConstants at: 'MONO_IMAGE_ERROR_ERRNO' put: 16r1!
MonoConstants at: 'MONO_IMAGE_IMAGE_INVALID' put: 16r3!
MonoConstants at: 'MONO_IMAGE_MISSING_ASSEMBLYREF' put: 16r2!
MonoConstants at: 'MONO_IMAGE_OK' put: 16r0!
MonoConstants at: 'MONO_METHOD_ATTR_ABSTRACT' put: 16r400!
MonoConstants at: 'MONO_METHOD_ATTR_ACCESS_MASK' put: 16r7!
MonoConstants at: 'MONO_METHOD_ATTR_ASSEM' put: 16r3!
MonoConstants at: 'MONO_METHOD_ATTR_COMPILER_CONTROLLED' put: 16r0!
MonoConstants at: 'MONO_METHOD_ATTR_FAM_AND_ASSEM' put: 16r2!
MonoConstants at: 'MONO_METHOD_ATTR_FAM_OR_ASSEM' put: 16r5!
MonoConstants at: 'MONO_METHOD_ATTR_FAMILY' put: 16r4!
MonoConstants at: 'MONO_METHOD_ATTR_FINAL' put: 16r20!
MonoConstants at: 'MONO_METHOD_ATTR_HAS_SECURITY' put: 16r4000!
MonoConstants at: 'MONO_METHOD_ATTR_HIDE_BY_SIG' put: 16r80!
MonoConstants at: 'MONO_METHOD_ATTR_NEW_SLOT' put: 16r100!
MonoConstants at: 'MONO_METHOD_ATTR_PINVOKE_IMPL' put: 16r2000!
MonoConstants at: 'MONO_METHOD_ATTR_PRIVATE' put: 16r1!
MonoConstants at: 'MONO_METHOD_ATTR_PUBLIC' put: 16r6!
MonoConstants at: 'MONO_METHOD_ATTR_REQUIRE_SEC_OBJECT' put: 16r8000!
MonoConstants at: 'MONO_METHOD_ATTR_RESERVED_MASK' put: 16rD000!
MonoConstants at: 'MONO_METHOD_ATTR_REUSE_SLOT' put: 16r0!
MonoConstants at: 'MONO_METHOD_ATTR_RT_SPECIAL_NAME' put: 16r1000!
MonoConstants at: 'MONO_METHOD_ATTR_SPECIAL_NAME' put: 16r800!
MonoConstants at: 'MONO_METHOD_ATTR_STATIC' put: 16r10!
MonoConstants at: 'MONO_METHOD_ATTR_STRICT' put: 16r200!
MonoConstants at: 'MONO_METHOD_ATTR_UNMANAGED_EXPORT' put: 16r8!
MonoConstants at: 'MONO_METHOD_ATTR_VIRTUAL' put: 16r40!
MonoConstants at: 'MONO_METHOD_ATTR_VTABLE_LAYOUT_MASK' put: 16r100!
MonoConstants at: 'MONO_TABLE_ASSEMBLY' put: 16r20!
MonoConstants at: 'MONO_TABLE_ASSEMBLYOS' put: 16r22!
MonoConstants at: 'MONO_TABLE_ASSEMBLYPROCESSOR' put: 16r21!
MonoConstants at: 'MONO_TABLE_ASSEMBLYREF' put: 16r23!
MonoConstants at: 'MONO_TABLE_ASSEMBLYREFOS' put: 16r25!
MonoConstants at: 'MONO_TABLE_ASSEMBLYREFPROCESSOR' put: 16r24!
MonoConstants at: 'MONO_TABLE_CLASSLAYOUT' put: 16rF!
MonoConstants at: 'MONO_TABLE_CONSTANT' put: 16rB!
MonoConstants at: 'MONO_TABLE_CUSTOMATTRIBUTE' put: 16rC!
MonoConstants at: 'MONO_TABLE_DECLSECURITY' put: 16rE!
MonoConstants at: 'MONO_TABLE_EVENT' put: 16r14!
MonoConstants at: 'MONO_TABLE_EVENT_POINTER' put: 16r13!
MonoConstants at: 'MONO_TABLE_EVENTMAP' put: 16r12!
MonoConstants at: 'MONO_TABLE_EXPORTEDTYPE' put: 16r27!
MonoConstants at: 'MONO_TABLE_FIELD' put: 16r4!
MonoConstants at: 'MONO_TABLE_FIELD_POINTER' put: 16r3!
MonoConstants at: 'MONO_TABLE_FIELDLAYOUT' put: 16r10!
MonoConstants at: 'MONO_TABLE_FIELDMARSHAL' put: 16rD!
MonoConstants at: 'MONO_TABLE_FIELDRVA' put: 16r1D!
MonoConstants at: 'MONO_TABLE_FILE' put: 16r26!
MonoConstants at: 'MONO_TABLE_GENERICPARAM' put: 16r2A!
MonoConstants at: 'MONO_TABLE_GENERICPARAMCONSTRAINT' put: 16r2C!
MonoConstants at: 'MONO_TABLE_IMPLMAP' put: 16r1C!
MonoConstants at: 'MONO_TABLE_INTERFACEIMPL' put: 16r9!
MonoConstants at: 'MONO_TABLE_LAST' put: 16r2C!
MonoConstants at: 'MONO_TABLE_MANIFESTRESOURCE' put: 16r28!
MonoConstants at: 'MONO_TABLE_MEMBERREF' put: 16rA!
MonoConstants at: 'MONO_TABLE_METHOD' put: 16r6!
MonoConstants at: 'MONO_TABLE_METHOD_POINTER' put: 16r5!
MonoConstants at: 'MONO_TABLE_METHODIMPL' put: 16r19!
MonoConstants at: 'MONO_TABLE_METHODSEMANTICS' put: 16r18!
MonoConstants at: 'MONO_TABLE_METHODSPEC' put: 16r2B!
MonoConstants at: 'MONO_TABLE_MODULE' put: 16r0!
MonoConstants at: 'MONO_TABLE_MODULEREF' put: 16r1A!
MonoConstants at: 'MONO_TABLE_NESTEDCLASS' put: 16r29!
MonoConstants at: 'MONO_TABLE_NUM' put: 16r2D!
MonoConstants at: 'MONO_TABLE_PARAM' put: 16r8!
MonoConstants at: 'MONO_TABLE_PARAM_POINTER' put: 16r7!
MonoConstants at: 'MONO_TABLE_PROPERTY' put: 16r17!
MonoConstants at: 'MONO_TABLE_PROPERTY_POINTER' put: 16r16!
MonoConstants at: 'MONO_TABLE_PROPERTYMAP' put: 16r15!
MonoConstants at: 'MONO_TABLE_STANDALONESIG' put: 16r11!
MonoConstants at: 'MONO_TABLE_TYPEDEF' put: 16r2!
MonoConstants at: 'MONO_TABLE_TYPEREF' put: 16r1!
MonoConstants at: 'MONO_TABLE_TYPESPEC' put: 16r1B!
MonoConstants at: 'MONO_TABLE_UNUSED6' put: 16r1E!
MonoConstants at: 'MONO_TABLE_UNUSED7' put: 16r1F!
MonoConstants at: 'MONO_TOKEN_ASSEMBLY' put: 16r20000000!
MonoConstants at: 'MONO_TOKEN_ASSEMBLY_REF' put: 16r23000000!
MonoConstants at: 'MONO_TOKEN_BASE_TYPE' put: 16r72000000!
MonoConstants at: 'MONO_TOKEN_CUSTOM_ATTRIBUTE' put: 16rC000000!
MonoConstants at: 'MONO_TOKEN_EVENT' put: 16r14000000!
MonoConstants at: 'MONO_TOKEN_EXPORTED_TYPE' put: 16r27000000!
MonoConstants at: 'MONO_TOKEN_FIELD_DEF' put: 16r4000000!
MonoConstants at: 'MONO_TOKEN_FILE' put: 16r26000000!
MonoConstants at: 'MONO_TOKEN_GENERIC_PARAM' put: 16r2A000000!
MonoConstants at: 'MONO_TOKEN_INTERFACE_IMPL' put: 16r9000000!
MonoConstants at: 'MONO_TOKEN_MANIFEST_RESOURCE' put: 16r28000000!
MonoConstants at: 'MONO_TOKEN_MEMBER_REF' put: 16rA000000!
MonoConstants at: 'MONO_TOKEN_METHOD_DEF' put: 16r6000000!
MonoConstants at: 'MONO_TOKEN_METHOD_SPEC' put: 16r2B000000!
MonoConstants at: 'MONO_TOKEN_MODULE' put: 16r0!
MonoConstants at: 'MONO_TOKEN_MODULE_REF' put: 16r1A000000!
MonoConstants at: 'MONO_TOKEN_NAME' put: 16r71000000!
MonoConstants at: 'MONO_TOKEN_PARAM_DEF' put: 16r8000000!
MonoConstants at: 'MONO_TOKEN_PERMISSION' put: 16rE000000!
MonoConstants at: 'MONO_TOKEN_PROPERTY' put: 16r17000000!
MonoConstants at: 'MONO_TOKEN_SIGNATURE' put: 16r11000000!
MonoConstants at: 'MONO_TOKEN_STRING' put: 16r70000000!
MonoConstants at: 'MONO_TOKEN_TYPE_DEF' put: 16r2000000!
MonoConstants at: 'MONO_TOKEN_TYPE_REF' put: 16r1000000!
MonoConstants at: 'MONO_TOKEN_TYPE_SPEC' put: 16r1B000000!
MonoConstants at: 'MONO_TYPE_ARRAY' put: 16r14!
MonoConstants at: 'MONO_TYPE_ATTR_ABSTRACT' put: 16r80!
MonoConstants at: 'MONO_TYPE_ATTR_ANSI_CLASS' put: 16r0!
MonoConstants at: 'MONO_TYPE_ATTR_AUTO_CLASS' put: 16r20000!
MonoConstants at: 'MONO_TYPE_ATTR_AUTO_LAYOUT' put: 16r0!
MonoConstants at: 'MONO_TYPE_ATTR_BEFORE_FIELD_INIT' put: 16r100000!
MonoConstants at: 'MONO_TYPE_ATTR_CLASS' put: 16r0!
MonoConstants at: 'MONO_TYPE_ATTR_CLASS_SEMANTIC_MASK' put: 16r20!
MonoConstants at: 'MONO_TYPE_ATTR_CUSTOM_CLASS' put: 16r30000!
MonoConstants at: 'MONO_TYPE_ATTR_CUSTOM_MASK' put: 16rC00000!
MonoConstants at: 'MONO_TYPE_ATTR_EXPLICIT_LAYOUT' put: 16r10!
MonoConstants at: 'MONO_TYPE_ATTR_FORWARDER' put: 16r200000!
MonoConstants at: 'MONO_TYPE_ATTR_HAS_SECURITY' put: 16r40000!
MonoConstants at: 'MONO_TYPE_ATTR_IMPORT' put: 16r1000!
MonoConstants at: 'MONO_TYPE_ATTR_INTERFACE' put: 16r20!
MonoConstants at: 'MONO_TYPE_ATTR_LAYOUT_MASK' put: 16r18!
MonoConstants at: 'MONO_TYPE_ATTR_NESTED_ASSEMBLY' put: 16r5!
MonoConstants at: 'MONO_TYPE_ATTR_NESTED_FAM_AND_ASSEM' put: 16r6!
MonoConstants at: 'MONO_TYPE_ATTR_NESTED_FAM_OR_ASSEM' put: 16r7!
MonoConstants at: 'MONO_TYPE_ATTR_NESTED_FAMILY' put: 16r4!
MonoConstants at: 'MONO_TYPE_ATTR_NESTED_PRIVATE' put: 16r3!
MonoConstants at: 'MONO_TYPE_ATTR_NESTED_PUBLIC' put: 16r2!
MonoConstants at: 'MONO_TYPE_ATTR_NOT_PUBLIC' put: 16r0!
MonoConstants at: 'MONO_TYPE_ATTR_PUBLIC' put: 16r1!
MonoConstants at: 'MONO_TYPE_ATTR_RESERVED_MASK' put: 16r40800!
MonoConstants at: 'MONO_TYPE_ATTR_RT_SPECIAL_NAME' put: 16r800!
MonoConstants at: 'MONO_TYPE_ATTR_SEALED' put: 16r100!
MonoConstants at: 'MONO_TYPE_ATTR_SEQUENTIAL_LAYOUT' put: 16r8!
MonoConstants at: 'MONO_TYPE_ATTR_SERIALIZABLE' put: 16r2000!
MonoConstants at: 'MONO_TYPE_ATTR_SPECIAL_NAME' put: 16r400!
MonoConstants at: 'MONO_TYPE_ATTR_STRING_FORMAT_MASK' put: 16r30000!
MonoConstants at: 'MONO_TYPE_ATTR_UNICODE_CLASS' put: 16r10000!
MonoConstants at: 'MONO_TYPE_ATTR_VISIBILITY_MASK' put: 16r7!
MonoConstants at: 'MONO_TYPE_BOOLEAN' put: 16r2!
MonoConstants at: 'MONO_TYPE_BYREF' put: 16r10!
MonoConstants at: 'MONO_TYPE_CHAR' put: 16r3!
MonoConstants at: 'MONO_TYPE_CLASS' put: 16r12!
MonoConstants at: 'MONO_TYPE_CMOD_OPT' put: 16r20!
MonoConstants at: 'MONO_TYPE_CMOD_REQD' put: 16r1F!
MonoConstants at: 'MONO_TYPE_END' put: 16r0!
MonoConstants at: 'MONO_TYPE_ENUM' put: 16r55!
MonoConstants at: 'MONO_TYPE_FNPTR' put: 16r1B!
MonoConstants at: 'MONO_TYPE_GENERICINST' put: 16r15!
MonoConstants at: 'MONO_TYPE_I' put: 16r18!
MonoConstants at: 'MONO_TYPE_I1' put: 16r4!
MonoConstants at: 'MONO_TYPE_I2' put: 16r6!
MonoConstants at: 'MONO_TYPE_I4' put: 16r8!
MonoConstants at: 'MONO_TYPE_I8' put: 16rA!
MonoConstants at: 'MONO_TYPE_INTERNAL' put: 16r21!
MonoConstants at: 'MONO_TYPE_MODIFIER' put: 16r40!
MonoConstants at: 'MONO_TYPE_MVAR' put: 16r1E!
MonoConstants at: 'MONO_TYPE_OBJECT' put: 16r1C!
MonoConstants at: 'MONO_TYPE_PINNED' put: 16r45!
MonoConstants at: 'MONO_TYPE_PTR' put: 16rF!
MonoConstants at: 'MONO_TYPE_R4' put: 16rC!
MonoConstants at: 'MONO_TYPE_R8' put: 16rD!
MonoConstants at: 'MONO_TYPE_SENTINEL' put: 16r41!
MonoConstants at: 'MONO_TYPE_STRING' put: 16rE!
MonoConstants at: 'MONO_TYPE_SZARRAY' put: 16r1D!
MonoConstants at: 'MONO_TYPE_TYPEDBYREF' put: 16r16!
MonoConstants at: 'MONO_TYPE_U' put: 16r19!
MonoConstants at: 'MONO_TYPE_U1' put: 16r5!
MonoConstants at: 'MONO_TYPE_U2' put: 16r7!
MonoConstants at: 'MONO_TYPE_U4' put: 16r9!
MonoConstants at: 'MONO_TYPE_U8' put: 16rB!
MonoConstants at: 'MONO_TYPE_VALUETYPE' put: 16r11!
MonoConstants at: 'MONO_TYPE_VAR' put: 16r13!
MonoConstants at: 'MONO_TYPE_VOID' put: 16r1!
MonoConstants shrink!

"Classes"!

MonoClassBuilder guid: (GUID fromString: '{791DA781-E5D9-44F3-BFA6-0D7264E758C0}')!
MonoClassBuilder comment: ''!
!MonoClassBuilder categoriesForClass!Kernel-Objects! !
!MonoClassBuilder methodsFor!

accessingCategory
	"Private - Answer the method category for accessor methods."

	^MethodCategory name: 'accessing'!

assemblyNameSelector
	"Private - Answer the assembly name selector."

	^#assemblyName!

autoGenCategory
	"Private - Answer the special method category for auto-generated methods."

	^MethodCategory name: '**auto generated**'!

build
	"Build the receiver's <MonoClass>."

	^self create!

canCreateClassNamed: aSymbol 
	"Private - Answer whether the aSymbol class name can be created."

	(self isValidClassName: aSymbol) 
		ifFalse: 
			[Notification signal: 'Not modifying class: ' , aSymbol.
			^false].
	(ClassBuilder isValidClassName: aSymbol) 
		ifFalse: 
			[monoClass isCompilerGenerated ifFalse: [Notification signal: 'Invalid class name: ' , aSymbol].
			^false].
	^true!

canGenerateMessageName: aString for: aClass 
	"Private - Answer whether the message selector constructed from aString can be safely
	generated into the receiver's associated wrapper class."

	| selector reservedSelectors |
	selector := (SelectorParser parse: aString) asSymbol.
	reservedSelectors := self reservedSelectorsFor: aClass.
	^(reservedSelectors includes: selector) not and: [(aClass includesSelector: selector) not]!

classNameSelector
	"Private - Answer the class name selector."

	^#className!

classNamespaceSelector
	"Private - Answer the class namespace selector."

	^#classNamespace!

compileAccessorCode: aString of: aMonoClassField 
	"Private - Compile aString method code and enter the result in the receiver's 'class' instance
	variable method dictionary. "

	| categories accessorClass messageName code |
	#todo.	"refactor this method"
	categories := Array with: self autoGenCategory with: self accessingCategory.
	accessorClass := aMonoClassField isClassVariable ifTrue: [class metaClass] ifFalse: [class].
	messageName := aString lines first.
	code := (self canGenerateMessageName: messageName for: accessorClass) 
				ifFalse: 
					[| reinventedMessageName |
					reinventedMessageName := self reinventBadMessageName: messageName for: accessorClass.
					self replaceMessageNameOf: aString with: reinventedMessageName]
				ifTrue: [aString].
	accessorClass compile: code categories: categories.
	^aString!

compileMethodCode: aString of: aMonoMethod 
	"Private - Compile aString method code and enter the result in the receiver's 'class' instance
	variable method dictionary. "

	| methodClass messageName code |
	#todo.	"refactor this method"
	methodClass := (aMonoMethod isClassMethod or: [aMonoMethod isConstructor]) 
				ifTrue: [class metaClass]
				ifFalse: [class].
	messageName := aString lines first.
	code := (self canGenerateMessageName: messageName for: methodClass) 
				ifFalse: 
					[| reinventedMessageName |
					reinventedMessageName := self reinventBadMessageName: messageName for: methodClass.
					self replaceMessageNameOf: aString with: reinventedMessageName]
				ifTrue: [aString].
	methodClass compile: code categories: (self methodCategoriesOf: aMonoMethod).
	^code!

compilePropertyCode: aString of: aMonoProperty 
	"Private - Compile aString method code and enter the result in the receiver's 'class' instance
	variable method dictionary. "

	| categories propertyClass messageName code |
	#todo.	"refactor this method"
	categories := OrderedCollection with: self autoGenCategory with: self accessingCategory.
	aMonoProperty isInterfaceImplementation 
		ifTrue: [categories add: (self interfaceImplCategoryOf: aMonoProperty)].
	propertyClass := aMonoProperty isClassProperty ifTrue: [class metaClass] ifFalse: [class].
	messageName := aString lines first.
	code := (self canGenerateMessageName: messageName for: propertyClass) 
				ifFalse: 
					[| reinventedMessageName |
					reinventedMessageName := self reinventBadMessageName: messageName for: propertyClass.
					self replaceMessageNameOf: aString with: reinventedMessageName]
				ifTrue: [aString].
	propertyClass compile: code categories: categories.
	^code!

convertMessageName: aMessageNameString for: aMonoProperty 
	"Private - Convert the method message name aMessageNameString to be aMonoProperty's
	message name."

	| skipSize |
	(skipSize := (aMessageNameString indexOf: $:) - 1) == -1 
		ifTrue: [skipSize := (aMessageNameString indexOf: Character cr) - 1].
	^aMonoProperty smalltalkName , (aMessageNameString allButFirst: skipSize)!

create
	"Private - Create the receiver's <MonoClass>."

	^monoClass isClass ifTrue: [self createClass] ifFalse: [self createInterface]!

createClass
	"Private - Create the Smalltalk class that wraps the receiver's <MonoClass>."

	| superclass className |
	superclass := monoClass smalltalkSuperclass.
	className := monoClass smalltalkClassName.
	(self canCreateClassNamed: className) ifFalse: [^nil].
	class := superclass 
				subclass: className
				instanceVariableNames: ''
				classVariableNames: ''
				poolDictionaries: ''
				classInstanceVariableNames: ''.
	class owningPackage: self package.
	self removeAutoGeneratedMethods.
	self generateMethods.
	self generateAccessors.
	^class!

createInterface
	"Private - Create the Smalltalk MethodProtocol that wraps the receiver's <MonoClass>."

	#todo.
	^nil
	"^self createClass"!

generateAccessor: aMonoClassField forGetter: aBoolean 
	"Private - Generate the accessor that wraps aMonoClassField."

	| code |
	code := String writeStream.
	self 
		printAccessorMessageNameOf: aMonoClassField
		on: code
		forGetter: aBoolean.
	self 
		printAccessorCommentOf: aMonoClassField
		on: code
		forGetter: aBoolean.
	self 
		printAccessorBodyOf: aMonoClassField
		on: code
		forGetter: aBoolean.
	self compileAccessorCode: code contents of: aMonoClassField!

generateAccessors
	"Private - Generate the receiver's <MonoClass> accessors."

	monoClass fields do: [:field | self generateAccessorsFor: field]!

generateAccessorsFor: aMonoClassField 
	"Private - Generate the accessors that wrap aMonoClassField."

	| isGetter |
	aMonoClassField isCompilerGenerated ifTrue: [^self].
	isGetter := false.
	2 timesRepeat: [self generateAccessor: aMonoClassField forGetter: (isGetter := isGetter not)]!

generateAssemblyNameMethod
	"Private - Generate the #assemblyName method."

	| assemblyNameSelector assemblyName code |
	assemblyNameSelector := self assemblyNameSelector.
	assemblyName := monoClass image name.
	[((Message selector: assemblyNameSelector) value: class) = assemblyName ifTrue: [^self]] 
		on: SubclassResponsibilityError
		do: [:ex | ].
	code := String writeStream.
	code
		nextPutAll: assemblyNameSelector;
		crtab;
		nextPutAll: '"Answer the assembly name to which the receiver belongs."';
		cr;
		crtab;
		nextPut: $^;
		print: assemblyName.
	class metaClass compile: code contents categories: (Array with: self autoGenCategory)!

generateClassNameMethod
	"Private - Generate the #className method."

	| code |
	code := String writeStream.
	code
		nextPutAll: self classNameSelector;
		crtab;
		nextPutAll: '"Answer the receiver''s class name."';
		cr;
		crtab;
		nextPut: $^;
		print: monoClass name.
	class metaClass compile: code contents categories: (Array with: self autoGenCategory)!

generateClassNamespaceMethod
	"Private - Generate the #classNamespace method."

	| code |
	code := String writeStream.
	code
		nextPutAll: self classNamespaceSelector;
		crtab;
		nextPutAll: '"Answer the receiver''s class namespace."';
		cr;
		crtab;
		nextPut: $^;
		print: monoClass namespace.
	class metaClass compile: code contents categories: (Array with: self autoGenCategory)!

generateConstructor: aMonoMethod 
	"Private - Generate the method that wraps aMonoMethod constructor method."

	| code |
	aMonoMethod isDefaultConstructor ifTrue: [^self].
	code := String writeStream.
	self printMethodMessageNameOf: aMonoMethod on: code.
	self printMethodCommentOf: aMonoMethod on: code.
	self printMethodHeaderOf: aMonoMethod on: code.
	code
		crtab;
		nextPutAll: 'object := self newInstance.';
		crtab;
		nextPutAll: 'method value: object withArguments: self methodArguments.';
		crtab;
		nextPutAll: '^object'.
	self compileMethodCode: code contents of: aMonoMethod!

generateMainMethods
	"Private - Generate the receiver's <MonoClass> main methods (#assemblyName, #className
	and #classNamespace)."

	self generateAssemblyNameMethod.
	self generateClassNameMethod.
	self generateClassNamespaceMethod!

generateMethod: aMonoMethod 
	"Private - Generate the method that wraps aMonoMethod."

	#todo.
	aMonoMethod isCompilerGenerated ifTrue: [^self].
	aMonoMethod isConstructor ifTrue: [^self generateConstructor: aMonoMethod].
	aMonoMethod isOperator ifTrue: [^self generateOperator: aMonoMethod].
	self generateStandardMethod: aMonoMethod!

generateMethods
	"Private - Generate the receiver's <MonoClass> methods."

	#todo.
	self generateMainMethods.
	monoClass methods do: [:method | self generateMethod: method]!

generateOperator: aMonoMethod 
	"Private - Generate the method that wraps aMonoMethod operator method."

	#todo!

generateProperty: aMonoProperty body: aString forGetter: aBoolean 
	"Private - Generate the property that wraps aMonoProperty."

	| method code |
	method := aBoolean ifTrue: [aMonoProperty getterMethod] ifFalse: [aMonoProperty setterMethod].
	method isNull ifTrue: [^self].
	code := String writeStream.
	self 
		printPropertyMessageNameOf: aMonoProperty
		on: code
		forGetter: aBoolean.
	self 
		printPropertyCommentOf: aMonoProperty
		on: code
		forGetter: aBoolean.
	self 
		printPropertyBody: aString
		of: aMonoProperty
		on: code
		forGetter: aBoolean.
	self compilePropertyCode: code contents of: aMonoProperty!

generateStandardMethod: aMonoMethod 
	"Private - Generate the method that wraps aMonoMethod."

	| code messageName |
	code := String writeStream.
	self printMethodMessageNameOf: aMonoMethod on: code.
	self printMethodCommentOf: aMonoMethod on: code.
	self printMethodHeaderOf: aMonoMethod on: code.
	self printMethodEvaluationOf: aMonoMethod on: code.
	self printMethodAnswerOn: code.
	messageName := (self compileMethodCode: code contents of: aMonoMethod) lines first.
	aMonoMethod isAccessor 
		ifTrue: 
			[self 
				generateProperty: aMonoMethod monoProperty
				body: messageName
				forGetter: aMonoMethod isGetter]!

instanceCreationCategory
	"Private - Answer the method category for instance creation methods."

	^MethodCategory name: 'instance creation'!

interfaceImplCategoryOf: aMonoMethodOrProperty 
	"Private - Answer the method category for interface implementation methods."

	^MethodCategory name: 'interface implementation-' , aMonoMethodOrProperty interfaceName!

isValidClassName: aSymbol 
	"Private - Answer whether the aSymbol class name is valid."

	^self class isValidClassName: aSymbol!

methodCategoriesOf: aMonoMethod 
	"Private - Answer the method categories of aMonoMethod."

	| categories |
	categories := OrderedCollection with: self autoGenCategory.
	aMonoMethod isConstructor ifTrue: [categories add: self instanceCreationCategory].
	aMonoMethod isInterfaceImplementation 
		ifTrue: [categories add: (self interfaceImplCategoryOf: aMonoMethod)].
	aMonoMethod isAccessor ifTrue: [categories add: self accessingCategory].
	^categories!

monoClass
	"Answer the receiver's monoClass instance variable."

	^monoClass!

monoClass: aMonoClass 
	"Set the receiver's monoClass instance variable to aMonoClass."

	monoClass := aMonoClass!

package
	"Private - Answer the package into which the receiver's wrapper class should be generated."

	| packageManager packageName |
	packageManager := Package manager.
	packageName := monoClass image smalltalkName.
	^packageManager packageNamed: packageName ifNone: [packageManager newPackage: packageName]!

printAccessorAnswerCommentOf: aMonoClassField on: aWriteStream 
	"Private - Print the answer comment of aMonoClassField on aWriteStream."

	aWriteStream
		cr;
		crtab: 2;
		nextPutAll: 'Answer';
		crtab: 3;
		nextPutAll: aMonoClassField monoType name!

printAccessorArgumentCommentOf: aMonoClassField on: aWriteStream 
	"Private - Print the argument comment of aMonoClassField on aWriteStream."

	| argumentTypeName |
	argumentTypeName := aMonoClassField monoType name.
	aWriteStream
		cr;
		crtab: 2;
		nextPutAll: 'Arguments';
		crtab: 3;
		nextPutAll: 'anObject';
		nextPutAll: ': ';
		nextPutAll: argumentTypeName!

printAccessorBodyOf: aMonoClassField on: aWriteStream forGetter: aBoolean 
	"Private - Print the accessor body of aMonoClassField on aWriteStream."

	aWriteStream
		cr;
		crtab;
		nextPutAll: '| field |';
		crtab;
		nextPutAll: 'field := self monoClass fieldNamed: ';
		print: aMonoClassField name;
		nextPut: $.;
		crtab.
	aBoolean 
		ifTrue: [aWriteStream nextPutAll: '^field valueFor: self']
		ifFalse: [aWriteStream nextPutAll: 'field value: anObject for: self']!

printAccessorCommentOf: aMonoClassField on: aWriteStream forGetter: aBoolean 
	"Private - Print the accessor comment of aMonoClassField on aWriteStream."

	#todo.	"refactor this method"
	aWriteStream
		crtab;
		nextPut: $".
	aMonoClassField isPublic ifFalse: [aWriteStream nextPutAll: 'Private - '].
	aWriteStream nextPutAll: (aBoolean ifTrue: ['Answer '] ifFalse: ['Set ']).
	aWriteStream
		nextPutAll: 'the value of the receiver''s ';
		print: aMonoClassField name.
	aMonoClassField isClassVariable 
		ifFalse: [aWriteStream nextPutAll: ' instance variable']
		ifTrue: [aWriteStream nextPutAll: ' class variable'].
	aBoolean 
		ifTrue: 
			[aWriteStream nextPut: $..
			self printAccessorAnswerCommentOf: aMonoClassField on: aWriteStream]
		ifFalse: 
			[aWriteStream nextPutAll: ' to anObject.'.
			self printAccessorArgumentCommentOf: aMonoClassField on: aWriteStream].
	aWriteStream nextPut: $"!

printAccessorMessageNameOf: aMonoClassField on: aWriteStream forGetter: aBoolean 
	"Private - Print the message name (keyword and arguments) of aMonoClassField on aWriteStream."

	aBoolean 
		ifTrue: [aWriteStream nextPutAll: aMonoClassField name]
		ifFalse: 
			[aWriteStream
				nextPutAll: aMonoClassField name;
				nextPut: $:;
				space;
				nextPutAll: 'anObject']!

printMethodAnswerCommentOf: aMonoMethod on: aWriteStream 
	"Private - Print the answer comment of aMonoMethod on aWriteStream."

	| returnType |
	aMonoMethod isConstructor ifTrue: [^self].
	returnType := aMonoMethod returnType.
	aWriteStream
		cr;
		crtab: 2;
		nextPutAll: 'Answer';
		crtab: 3;
		nextPutAll: (returnType isVoid ifTrue: ['self'] ifFalse: [returnType name])!

printMethodAnswerOn: aWriteStream 
	"Private - Print the method's answer on aWriteStream."

	aWriteStream
		crtab;
		nextPutAll: '^method returnType isVoid ifTrue: [self] ifFalse: [object]'!

printMethodArgumentsCommentOf: aMonoMethod on: aWriteStream 
	"Private - Print the arguments comment of aMonoMethod on aWriteStream."

	| argumentTypes argumentNames |
	aMonoMethod argumentCount isZero ifTrue: [^self].
	argumentTypes := aMonoMethod argumentTypes.
	argumentNames := self reinventBadArgumentNamesOf: aMonoMethod.
	aWriteStream
		cr;
		crtab: 2;
		nextPutAll: 'Arguments'.
	argumentTypes with: argumentNames
		do: 
			[:argumentType :argumentName | 
			aWriteStream
				crtab: 3;
				nextPutAll: argumentName;
				nextPutAll: ': ';
				nextPutAll: argumentType name]!

printMethodCommentOf: aMonoMethod on: aWriteStream 
	"Private - Print the method comment of aMonoMethod on aWriteStream."

	| privateString |
	#todo.	"refactor this method"
	privateString := 'Private - '.
	aWriteStream nextPut: $".
	(aMonoMethod isPublic not or: [aMonoMethod isAccessor]) 
		ifTrue: [aWriteStream nextPutAll: privateString].
	aMonoMethod isConstructor 
		ifTrue: [aWriteStream nextPutAll: 'Answer a new initialized instance of the receiver.']
		ifFalse: 
			[aWriteStream nextPutAll: (aMonoMethod isAccessor 
						ifTrue: 
							[| stream accessorComment |
							stream := String writeStream.
							self 
								printPropertyCommentOf: aMonoMethod monoProperty
								on: stream
								forGetter: aMonoMethod isGetter.
							^((accessorComment := stream contents) beginsWith: '"' , privateString) 
								ifTrue: [aWriteStream nextPutAll: (accessorComment allButFirst: privateString size + 1)]
								ifFalse: [aWriteStream nextPutAll: accessorComment allButFirst]]
						ifFalse: ['Automatically generated method.'])].
	self printMethodArgumentsCommentOf: aMonoMethod on: aWriteStream.
	self printMethodAnswerCommentOf: aMonoMethod on: aWriteStream.
	aWriteStream nextPut: $"!

printMethodEvaluationOf: aMonoMethod on: aWriteStream 
	"Private - Print the evaluation statement of aMonoMethod on aWriteStream."

	| argumentCount |
	argumentCount := aMonoMethod argumentCount.
	aWriteStream
		crtab;
		nextPutAll: 'object := '.
	aMonoMethod isClassMethod 
		ifTrue: 
			[argumentCount isZero 
				ifTrue: [aWriteStream nextPutAll: 'method value.']
				ifFalse: [aWriteStream nextPutAll: 'method valueWithArguments: self methodArguments.']]
		ifFalse: 
			[argumentCount isZero 
				ifTrue: [aWriteStream nextPutAll: 'method value: self.']
				ifFalse: [aWriteStream nextPutAll: 'method value: self withArguments: self methodArguments.']]!

printMethodHeaderOf: aMonoMethod on: aWriteStream 
	"Private - Print the method header (local variable declaration and method search) of
	aMonoMethod on aWriteStream."

	aWriteStream
		cr;
		crtab;
		nextPutAll: '| method object |';
		crtab;
		nextPutAll: 'method := self monoClass methodNamed: ';
		print: aMonoMethod fullNameWithSignature;
		nextPut: $.!

printMethodMessageNameOf: aMonoMethod on: aWriteStream 
	"Private - Print the message name (keyword and arguments) of aMonoMethod on aWriteStream."

	| keywords argumentNames |
	#todo.	"refactor this method"
	keywords := ReadStream on: aMonoMethod argumentNames.
	argumentNames := ReadStream on: (self reinventBadArgumentNamesOf: aMonoMethod).
	aWriteStream nextPutAll: aMonoMethod smalltalkName.
	aMonoMethod argumentCount isZero 
		ifFalse: 
			[aWriteStream
				nextPut: $:;
				space;
				nextPutAll: argumentNames next.
			keywords next].
	[keywords atEnd] whileFalse: 
			[| selectorToken |
			selectorToken := keywords peek isEmpty 
						ifTrue: 
							[#bug.	"Mono bug? See #reinventBadArgumentNamesOf:"
							keywords next.
							argumentNames peek]
						ifFalse: [keywords next].
			aWriteStream
				space;
				nextPutAll: selectorToken;
				nextPut: $:;
				space;
				nextPutAll: argumentNames next].
	aWriteStream crtab!

printPropertyAnswerCommentOf: aMonoProperty on: aWriteStream 
	"Private - Print the answer comment of aMonoProperty on aWriteStream."

	| returnTypeName |
	returnTypeName := aMonoProperty getterMethod returnType name.
	aWriteStream
		cr;
		crtab: 2;
		nextPutAll: 'Answer';
		crtab: 3;
		nextPutAll: returnTypeName!

printPropertyBody: aString of: aMonoProperty on: aWriteStream forGetter: aBoolean 
	"Private - Print the property body of aMonoProperty on aWriteStream."

	aWriteStream
		cr;
		crtab.
	aBoolean ifTrue: [aWriteStream nextPut: $^].
	aWriteStream
		nextPutAll: 'self ';
		nextPutAll: aString!

printPropertyCommentOf: aMonoProperty on: aWriteStream forGetter: aBoolean 
	"Private - Print the property comment of aMonoProperty on aWriteStream."

	| method |
	#todo.	"refactor this method"
	method := aBoolean ifTrue: [aMonoProperty getterMethod] ifFalse: [aMonoProperty setterMethod].
	aWriteStream nextPut: $".
	method isPublic ifFalse: [aWriteStream nextPutAll: 'Private - '].
	aWriteStream nextPutAll: (aBoolean ifTrue: ['Answer '] ifFalse: ['Set ']).
	aWriteStream
		nextPutAll: 'the value of the receiver''s ';
		print: aMonoProperty smalltalkName.	"todo: print the property's .NET friendly name"
	aMonoProperty isClassProperty 
		ifFalse: [aWriteStream nextPutAll: ' instance property']
		ifTrue: [aWriteStream nextPutAll: ' class property'].
	aBoolean 
		ifTrue: 
			[method argumentCount isZero 
				ifFalse: 
					[aWriteStream
						nextPutAll: ' according to the argument, ';
						nextPutAll: method argumentNames first]]
		ifFalse: 
			[method argumentCount > 1 
				ifTrue: [aWriteStream nextPutAll: ' according to the arguments']
				ifFalse: 
					[aWriteStream
						nextPutAll: ' to the argument, ';
						nextPutAll: method argumentNames first]].
	aWriteStream nextPut: $..
	self printMethodArgumentsCommentOf: method on: aWriteStream.
	aBoolean ifTrue: [self printPropertyAnswerCommentOf: aMonoProperty on: aWriteStream].
	aWriteStream nextPut: $"!

printPropertyMessageNameOf: aMonoProperty on: aWriteStream forGetter: aBoolean 
	"Private - Print the message name (keyword and arguments) of aMonoProperty on aWriteStream."

	| messageName method |
	messageName := String writeStream.
	method := aBoolean ifTrue: [aMonoProperty getterMethod] ifFalse: [aMonoProperty setterMethod].
	self printMethodMessageNameOf: method on: messageName.
	aWriteStream nextPutAll: (self convertMessageName: messageName contents for: aMonoProperty)!

reinventBadArgumentNamesOf: aMonoMethod 
	"Private - Answer a collection of aMonoMethod's argument names that don't clash with our
	wrapped method local variable names."

	| count badArgumentNames |
	count := 0.
	badArgumentNames := #('method' 'object').
	^aMonoMethod argumentNames collect: 
			[:argumentName | 
			argumentName isEmpty 
				ifTrue: 
					[#bug.	"Mono bug?"
					count := count + 1.
					'arg' , count printString]
				ifFalse: 
					[(badArgumentNames includes: argumentName) ifTrue: [argumentName , '_'] ifFalse: [argumentName]]]!

reinventBadMessageName: aString for: aClass 
	"Private - If the selector constructed from the message name (keyword and arguments) in
	aString would cause a clash with a selector in aClass, then reinvent it."

	| messageArray stream messageName |
	#todo.	"refactor this method"
	messageArray := aString subStrings: Character space.
	stream := String writeStream.
	messageArray size == 1 
		ifTrue: 
			[stream
				nextPutAll: messageArray first;
				nextPut: $_]
		ifFalse: 
			[stream
				nextPutAll: messageArray first allButLast;
				nextPutAll: '_: '.
			messageArray allButFirst
				do: [:token | stream nextPutAll: token]
				separatedBy: [stream nextPutAll: ' ']].
	messageName := stream contents.
	^(self canGenerateMessageName: messageName for: aClass) 
		ifTrue: [messageName]
		ifFalse: [self reinventBadMessageName: messageName for: aClass]!

removeAutoGeneratedMethods
	"Private - Remove any auto-generated methods from the class and instance side of the class
	generated by the receiver."

	| autoGenCategoryName |
	autoGenCategoryName := self autoGenCategory name.
	class removeCategory: autoGenCategoryName.
	class metaClass removeCategory: autoGenCategoryName!

replaceMessageNameOf: aMethodCodeString with: aMessageNameString 
	"Private - Replace the message name of aMethodCodeString with aMessageNameString."

	^aMessageNameString , (aMethodCodeString allButFirst: (aMethodCodeString indexOf: Character cr) - 1)!

reservedSelectorsFor: aClass 
	"Private - Answer a <Set> of all the selectors which are reserved for aClass, and should not
	be overridden by any auto-generated method."

	^aClass isMeta 
		ifTrue: [self class reservedClassSelectors]
		ifFalse: [self class reservedInstanceSelectors]! !
!MonoClassBuilder categoriesFor: #accessingCategory!private! !
!MonoClassBuilder categoriesFor: #assemblyNameSelector!constants!private! !
!MonoClassBuilder categoriesFor: #autoGenCategory!private! !
!MonoClassBuilder categoriesFor: #build!public! !
!MonoClassBuilder categoriesFor: #canCreateClassNamed:!private!testing! !
!MonoClassBuilder categoriesFor: #canGenerateMessageName:for:!private!testing! !
!MonoClassBuilder categoriesFor: #classNameSelector!constants!private! !
!MonoClassBuilder categoriesFor: #classNamespaceSelector!constants!private! !
!MonoClassBuilder categoriesFor: #compileAccessorCode:of:!private! !
!MonoClassBuilder categoriesFor: #compileMethodCode:of:!private! !
!MonoClassBuilder categoriesFor: #compilePropertyCode:of:!private! !
!MonoClassBuilder categoriesFor: #convertMessageName:for:!private! !
!MonoClassBuilder categoriesFor: #create!private! !
!MonoClassBuilder categoriesFor: #createClass!private! !
!MonoClassBuilder categoriesFor: #createInterface!private! !
!MonoClassBuilder categoriesFor: #generateAccessor:forGetter:!private! !
!MonoClassBuilder categoriesFor: #generateAccessors!private! !
!MonoClassBuilder categoriesFor: #generateAccessorsFor:!private! !
!MonoClassBuilder categoriesFor: #generateAssemblyNameMethod!private! !
!MonoClassBuilder categoriesFor: #generateClassNameMethod!private! !
!MonoClassBuilder categoriesFor: #generateClassNamespaceMethod!private! !
!MonoClassBuilder categoriesFor: #generateConstructor:!private! !
!MonoClassBuilder categoriesFor: #generateMainMethods!private! !
!MonoClassBuilder categoriesFor: #generateMethod:!private! !
!MonoClassBuilder categoriesFor: #generateMethods!private! !
!MonoClassBuilder categoriesFor: #generateOperator:!private! !
!MonoClassBuilder categoriesFor: #generateProperty:body:forGetter:!private! !
!MonoClassBuilder categoriesFor: #generateStandardMethod:!private! !
!MonoClassBuilder categoriesFor: #instanceCreationCategory!private! !
!MonoClassBuilder categoriesFor: #interfaceImplCategoryOf:!private! !
!MonoClassBuilder categoriesFor: #isValidClassName:!private! !
!MonoClassBuilder categoriesFor: #methodCategoriesOf:!private! !
!MonoClassBuilder categoriesFor: #monoClass!accessing!public! !
!MonoClassBuilder categoriesFor: #monoClass:!accessing!public! !
!MonoClassBuilder categoriesFor: #package!accessing!private! !
!MonoClassBuilder categoriesFor: #printAccessorAnswerCommentOf:on:!private! !
!MonoClassBuilder categoriesFor: #printAccessorArgumentCommentOf:on:!private! !
!MonoClassBuilder categoriesFor: #printAccessorBodyOf:on:forGetter:!private! !
!MonoClassBuilder categoriesFor: #printAccessorCommentOf:on:forGetter:!private! !
!MonoClassBuilder categoriesFor: #printAccessorMessageNameOf:on:forGetter:!private! !
!MonoClassBuilder categoriesFor: #printMethodAnswerCommentOf:on:!private! !
!MonoClassBuilder categoriesFor: #printMethodAnswerOn:!private! !
!MonoClassBuilder categoriesFor: #printMethodArgumentsCommentOf:on:!private! !
!MonoClassBuilder categoriesFor: #printMethodCommentOf:on:!private! !
!MonoClassBuilder categoriesFor: #printMethodEvaluationOf:on:!private! !
!MonoClassBuilder categoriesFor: #printMethodHeaderOf:on:!private! !
!MonoClassBuilder categoriesFor: #printMethodMessageNameOf:on:!private! !
!MonoClassBuilder categoriesFor: #printPropertyAnswerCommentOf:on:!private! !
!MonoClassBuilder categoriesFor: #printPropertyBody:of:on:forGetter:!private! !
!MonoClassBuilder categoriesFor: #printPropertyCommentOf:on:forGetter:!private! !
!MonoClassBuilder categoriesFor: #printPropertyMessageNameOf:on:forGetter:!private! !
!MonoClassBuilder categoriesFor: #reinventBadArgumentNamesOf:!private! !
!MonoClassBuilder categoriesFor: #reinventBadMessageName:for:!private! !
!MonoClassBuilder categoriesFor: #removeAutoGeneratedMethods!private! !
!MonoClassBuilder categoriesFor: #replaceMessageNameOf:with:!private! !
!MonoClassBuilder categoriesFor: #reservedSelectorsFor:!private! !

!MonoClassBuilder class methodsFor!

isValidClassName: aSymbol 
	"Private - Answer whether the aSymbol class name is valid."

	| class |
	class := Smalltalk at: aSymbol ifAbsent: [^true].
	^class isMonoObject and: [class ~~ MonoObject and: [class ~~ MonoArray and: [class ~~ MonoString]]]!

on: aMonoClass 
	"Answer a new instance of the receiver on aMonoClass."

	^self new monoClass: aMonoClass!

reservedClassSelectors
	"Private - Answer a <Set> of all the class selectors which are reserved, and should not be
	overridden by any auto-generated method."

	| reservedClassSelectors additionalClassSelectors |
	reservedClassSelectors := MonoObject metaClass selectors.
	additionalClassSelectors := #(#categories #categories:).
	^reservedClassSelectors
		addAll: additionalClassSelectors;
		yourself!

reservedInstanceSelectors
	"Private - Answer a <Set> of all the instance selectors which are reserved, and should not be
	overridden by any auto-generated method."

	| reservedInstanceSelectors optimizedSelectors |
	reservedInstanceSelectors := MonoObject selectors.
	optimizedSelectors := AXInterfaceTypeAnalyzer optimizedSelectors.
	^reservedInstanceSelectors
		remove: #=;
		addAll: optimizedSelectors;
		yourself! !
!MonoClassBuilder class categoriesFor: #isValidClassName:!private! !
!MonoClassBuilder class categoriesFor: #on:!public! !
!MonoClassBuilder class categoriesFor: #reservedClassSelectors!private! !
!MonoClassBuilder class categoriesFor: #reservedInstanceSelectors!private! !

MonoMemoryManager guid: (GUID fromString: '{0BB15829-5C70-41F5-B59D-B3839726694F}')!
MonoMemoryManager comment: ''!
!MonoMemoryManager categoriesForClass!Kernel-Objects! !
!MonoMemoryManager methodsFor!

collectGarbage
	"Invoke the garbage collector to reclaim memory that is inaccessible."

	self collectGarbage: self maxGeneration!

collectGarbage: anInteger 
	"Private - Invoke the garbage collector to collect objects with anInteger generation number
	or below."

	MonoLibrary default monoGCCollect: anInteger!

heapSize
	"Answer the receiver's heap size."

	^MonoLibrary default monoGCGetHeapSize!

maxGeneration
	"Answer the maximum number of generations that the system currently supports."

	^MonoLibrary default monoGCMaxGeneration!

usedSize
	"Answer the receiver's used size."

	^MonoLibrary default monoGCGetUsedSize! !
!MonoMemoryManager categoriesFor: #collectGarbage!garbage collection!public! !
!MonoMemoryManager categoriesFor: #collectGarbage:!garbage collection!private! !
!MonoMemoryManager categoriesFor: #heapSize!public! !
!MonoMemoryManager categoriesFor: #maxGeneration!public! !
!MonoMemoryManager categoriesFor: #usedSize!public! !

!MonoMemoryManager class methodsFor!

current
	"Answer the singleton instance of the receiver."

	^Current ifNil: [Current := super new]!

new
	"The receiver is a singleton, use #current."

	^self shouldNotImplement! !
!MonoMemoryManager class categoriesFor: #current!instance creation!public! !
!MonoMemoryManager class categoriesFor: #new!instance creation!public! !

MonoRuntime guid: (GUID fromString: '{77621932-65EC-48B4-9358-3994F19E08C6}')!
MonoRuntime comment: ''!
!MonoRuntime categoriesForClass!Kernel-Objects! !
!MonoRuntime methodsFor!

assemblyPath
	"Answer the receiver's base directory for assemblies."

	^assemblyPath!

assemblyPath: aString 
	"Set the receiver's base directory for assemblies to aString."

	(File isDirectory: aString) ifFalse: [^self error: 'Invalid assembly path: ' , aString].
	assemblyPath := aString!

configurationFile
	"Answer the receiver's configuration filename."

	^configurationFile!

configurationFile: aString 
	"Set the receiver's configuration filename to aString"

	configurationFile := aString!

configurationPath
	"Answer the receiver's base directory for configuration files."

	^configurationPath!

configurationPath: aString 
	"Set the receiver's base directory for configuration files to aString."

	(File isDirectory: aString) ifFalse: [^self error: 'Invalid configuration path: ' , aString].
	configurationPath := aString!

defaultVersion
	"Private - Answer the receiver's default version number."

	^4!

domain
	"Answer the current <MonoDomain>."

	^domain!

domainName
	"Answer the name of the main application domain."

	^domainName ifNil: [File splitFilenameFrom: SessionManager current argv first]!

domainName: aString 
	"Set the name of the main application domain to aString."

	domainName := aString!

exitCode
	"Answer the return value of the Main() method in the assembly."

	^monoLibrary monoEnvironmentExitcodeGet!

initialize
	"Initialize and configure the Mono runtime."

	#todo.
	domain notNil ifTrue: [^Notification signal: 'Mono runtime already initialized'].
	monoLibrary := MonoLibrary default.
	self setMonoPath.
	monoLibrary
		monoSetDirs: (File unixPathOf: self assemblyPath)
		configDir: (File unixPathOf: self configurationPath).
	self loadConfigurationFrom: self configurationFile.
	"domain := monoLibrary monoJITInit: (File splitFilenameFrom: SessionManager current argv first)"
	domain := monoLibrary monoJITInitVersion: self domainName runtimeVersion: self longVersion
	"self installAssemblyPreloadHook"!

installAssemblyPreloadHook
	"Private - Install the assembly preload hook."

	assemblyPreloadHookCallback := ExternalCallback block: 
					[:assemblyName :assembliesPath :userData | 
					self halt.
					nil]
				descriptor: (ExternalDescriptor 
						callingConvention: 'cdecl:'
						returnType: 'MonoAssembly*'
						argumentTypes: 'MonoAssemblyName* char** void*').
	MonoLibrary default monoInstallAssemblyPreloadHook: assemblyPreloadHookCallback asParameter userData: nil!

loadConfigurationFrom: filename 
	"Private - Load the Mono configuration file, filename."

	(filename notNil and: [(File exists: self configurationPath , '\' , filename) not]) 
		ifTrue: [^self error: 'Invalid configuration file: ' , filename].
	monoLibrary monoConfigParse: filename!

loadDefaultConfiguration
	"Private - Load the default configuration file."

	self loadConfigurationFrom: nil!

longVersion
	"Answer the receiver's long version."

	^self supportedVersions at: self version!

mscorlib
	"Answer the mscorlib assembly image."

	^MonoImage mscorlib!

setMonoPath
	"Private - Set the MONO_PATH environment variable."

	| stream paths |
	stream := String writeStream.
	paths := Set 
				with: (File unixPathOf: self assemblyPath , '\mono\' , self shortVersion)
				with: (File unixPathOf: (File splitPathFrom: SessionManager current argv first))
				with: (File unixPathOf: File workingDirectory)
				with: (File unixPathOf: SessionManager current installationDirectory)
				with: (File unixPathOf: SessionManager current imageBase).
	paths do: [:path | stream nextPutAll: path] separatedBy: [stream nextPut: $;].
	SessionManager current setenv: 'MONO_PATH' value: stream contents!

shortVersion
	"Answer the receiver's short version."

	^self longVersion copyFrom: 2 to: 4!

shutdown
	"Shutdown the Mono runtime."

	monoLibrary monoJITCleanup: self domain!

supportedVersions
	"Answer a dictionary with the receiver's supported versions."

	^Dictionary with: 2 -> 'v2.0.50727' with: 4 -> 'v4.0.30319'!

version
	"Answer the receiver's version number."

	^version ifNil: [version := self defaultVersion]!

version: anInteger 
	"Set the receiver's version number to anInteger."

	(self supportedVersions keys includes: anInteger) 
		ifFalse: [^self error: 'Invalid runtime version: ' , anInteger printString].
	version := anInteger! !
!MonoRuntime categoriesFor: #assemblyPath!accessing!public! !
!MonoRuntime categoriesFor: #assemblyPath:!accessing!public! !
!MonoRuntime categoriesFor: #configurationFile!accessing!public! !
!MonoRuntime categoriesFor: #configurationFile:!accessing!public! !
!MonoRuntime categoriesFor: #configurationPath!accessing!public! !
!MonoRuntime categoriesFor: #configurationPath:!accessing!public! !
!MonoRuntime categoriesFor: #defaultVersion!private! !
!MonoRuntime categoriesFor: #domain!accessing!public! !
!MonoRuntime categoriesFor: #domainName!accessing!public! !
!MonoRuntime categoriesFor: #domainName:!accessing!public! !
!MonoRuntime categoriesFor: #exitCode!accessing!public! !
!MonoRuntime categoriesFor: #initialize!public! !
!MonoRuntime categoriesFor: #installAssemblyPreloadHook!private! !
!MonoRuntime categoriesFor: #loadConfigurationFrom:!private! !
!MonoRuntime categoriesFor: #loadDefaultConfiguration!private! !
!MonoRuntime categoriesFor: #longVersion!public! !
!MonoRuntime categoriesFor: #mscorlib!public! !
!MonoRuntime categoriesFor: #setMonoPath!private! !
!MonoRuntime categoriesFor: #shortVersion!public! !
!MonoRuntime categoriesFor: #shutdown!public! !
!MonoRuntime categoriesFor: #supportedVersions!public! !
!MonoRuntime categoriesFor: #version!accessing!public! !
!MonoRuntime categoriesFor: #version:!accessing!public! !

!MonoRuntime class methodsFor!

current
	"Answer the singleton instance of the receiver."

	^Current ifNil: [Current := super new]!

new
	"The receiver is a singleton, use #current."

	^self shouldNotImplement! !
!MonoRuntime class categoriesFor: #current!instance creation!public! !
!MonoRuntime class categoriesFor: #new!instance creation!public! !

MonoLibrary guid: (GUID fromString: '{B39E88B8-3808-4D66-BEDB-97F12A9C29A8}')!
MonoLibrary comment: ''!
!MonoLibrary categoriesForClass!External-Libraries! !
!MonoLibrary methodsFor!

monoArrayAddrWithSize: aMonoArray size: anIntegerSize idx: anIntegerIndex 
	"Returns the address of the anIntegerIndex element in aMonoArray.

	Implementation Note: The return type is not void*, but we need an <ExternalAddress>.

		char* mono_array_addr_with_size(
			MonoArray *array,
			int size,
			uintptr_t idx
		);"

	<stdcall: void* mono_array_addr_with_size MonoArray* sdword dword>
	self invalidCall!

monoArrayClone: aMonoArray 
	"Returns a newly created <MonoArray> who is a shallow copy of aMonoArray.

		MonoArray* mono_array_clone(MonoArray *array);"

	<stdcall: MonoArray* mono_array_clone MonoArray*>
	self invalidCall!

monoArrayElementSize: aMonoClass 
	"Returns the size of single array element in aMonoClass.

		int32_t mono_array_element_size(MonoClass *ac);"

	<stdcall: sdword mono_array_element_size MonoClass*>
	self invalidCall!

monoArrayLength: aMonoArray 
	"Returns the total number of elements in aMonoArray.
	This works for both vectors and multidimensional arrays. 

		uintptr_t mono_array_length(MonoArray *array);"

	<stdcall: dword mono_array_length MonoArray*>
	self invalidCall!

monoArrayNew: aMonoDomain eclass: aMonoClass n: anInteger 
	"Returns a <MonoArray> with anInteger elements of type aMonoClass.

		MonoArray* mono_array_new(
			MonoDomain *domain,
			MonoClass *eclass,
			uintptr_t n
		);"

	<stdcall: MonoArray* mono_array_new MonoDomain* MonoClass* dword>
	self invalidCall!

monoAssemblyGetImage: aMonoAssembly 
	"Return the MonoImage associated with the specified assembly.

		MonoImage* mono_assembly_get_image(MonoAssembly *assembly);"

	<stdcall: MonoImage* mono_assembly_get_image MonoAssembly*>
	self invalidCall!

monoAssemblyLoad: aMonoAssemblyName basedir: basedir status: status 
	"Loads the assembly referenced by aMonoAssemblyName.

		MonoAssembly* mono_assembly_load(
			MonoAssemblyName *aname,
			const char *basedir,
			MonoImageOpenStatus *status
		);"

	<stdcall: MonoAssembly* mono_assembly_load MonoAssemblyName* char* sdword*>
	self invalidCall!

monoAssemblyLoaded: aMonoAssemblyName 
	"Returns NULL If the given assembly name has not been loaded, or a pointer to a
	MonoAssembly that matches the MonoAssemblyName specified.

		MonoAssembly* mono_assembly_loaded(MonoAssemblyName *aname);"

	<stdcall: MonoAssembly* mono_assembly_loaded MonoAssemblyName*>
	self invalidCall!

monoAssemblyNameFree: aMonoAssemblyName 
	"Frees the provided assembly name object (it does not frees the object itself, only the
	name members).

		void mono_assembly_name_free(MonoAssemblyName *aname);"

	<stdcall: void mono_assembly_name_free MonoAssemblyName*>
	self invalidCall!

monoAssemblyNameGetName: aMonoAssemblyName 
	"

		const char* mono_assembly_name_get_name(MonoAssemblyName *aname);"

	<stdcall: char* mono_assembly_name_get_name MonoAssemblyName*>
	self invalidCall!

monoAssemblyNameNew: aString 
	"

		MonoAssemblyName* mono_assembly_name_new(const char *name);"

	<stdcall: MonoAssemblyName* mono_assembly_name_new char*>
	self invalidCall!

monoAssemblyNamesEqual: aMonoAssemblyName1 r: aMonoAssemblyName2 
	"Answer whether two <MonoAssemblyName>s are equal.

		mono_bool mono_assembly_names_equal(
			MonoAssemblyName *l,
			MonoAssemblyName *r
		);"

	<stdcall: bool mono_assembly_names_equal MonoAssemblyName* MonoAssemblyName*>
	self invalidCall!

monoAssemblyOpen: filename status: status 
	"Opens the PE-image pointed by filename, and loads any external assemblies referenced by it.

		MonoAssembly* mono_assembly_open(
			const char *filename,
			MonoImageOpenStatus *status
		);"

	<stdcall: MonoAssembly* mono_assembly_open char* sdword*>
	self invalidCall!

monoClassFromMonoType: aMonoType 
	"Returns the <MonoClass> that describes the class that aMonoType represents. 

		MonoClass* mono_class_from_mono_type(MonoType *type);"

	<stdcall: MonoClass* mono_class_from_mono_type MonoType*>
	self invalidCall!

monoClassFromName: aMonoImage namespace: namespace name: name 
	"Obtains a MonoClass with a given namespace and a given name which is located
	in the given MonoImage.

		MonoClass* mono_class_from_name(
			MonoImage *image,
			const char* name_space,
			const char *name
		);"

	<stdcall: MonoClass* mono_class_from_name MonoImage* char* char*>
	self invalidCall!

monoClassGet: aMonoImage typeToken: typeToken 
	"

		MonoClass* mono_class_get(
			MonoImage *image,
			uint32_t type_token
		);"

	<stdcall: MonoClass* mono_class_get MonoImage* dword>
	self invalidCall!

monoClassGetCctor: aMonoClass 
	"Answer the static constructor of aMonoClass.

		MonoMethod* mono_class_get_cctor(MonoClass *klass);"

	<stdcall: MonoMethod* mono_class_get_cctor MonoClass*>
	self invalidCall!

monoClassGetElementClass: aMonoClass 
	"Returns the element class of an array or an enumeration.

		MonoClass* mono_class_get_element_class(MonoClass *klass);"

	<stdcall: MonoClass* mono_class_get_element_class MonoClass*>
	self invalidCall!

monoClassGetFieldFromName: aMonoClass name: aString 
	"Search the <MonoClassField> named aString in aMonoClass and it's parents.

		MonoClassField *mono_class_get_field_from_name(
			MonoClass *klass,
			const char *name
		);"

	<stdcall: MonoClassField* mono_class_get_field_from_name MonoClass* char*>
	self invalidCall!

monoClassGetFields: aMonoClass iter: iter 
	"

		MonoClassField* mono_class_get_fields(
			MonoClass* klass,
			void **iter
		);"

	<stdcall: MonoClassField* mono_class_get_fields MonoClass* void**>
	self invalidCall!

monoClassGetFlags: aMonoClass 
	"Returns the type flags from the TypeDef table from the metadata.

		uint32_t mono_class_get_flags(MonoClass *klass);"

	<stdcall: dword mono_class_get_flags MonoClass*>
	self invalidCall!

monoClassGetImage: aMonoClass 
	"

		MonoImage* mono_class_get_image(MonoClass *klass);"

	<stdcall: MonoImage* mono_class_get_image MonoClass*>
	self invalidCall!

monoClassGetInterfaces: aMonoClass iter: iter 
	"

		MonoClass* mono_class_get_interfaces(MonoClass* klass, void **iter);"

	<stdcall: MonoClass* mono_class_get_interfaces MonoClass* void**>
	self invalidCall!

monoClassGetMethods: aMonoClass iter: iter 
	"Returns a MonoMethod on each iteration or NULL when no more methods are available.

		MonoMethod* mono_class_get_methods(
			MonoClass* klass,
			void **iter
		);"

	<stdcall: MonoMethod* mono_class_get_methods MonoClass* void**>
	self invalidCall!

monoClassGetName: aMonoClass 
	"Returns the name of aMonoClass.

		const char* mono_class_get_name (MonoClass *klass);"

	<stdcall: char* mono_class_get_name MonoClass*>
	self invalidCall!

monoClassGetNamespace: aMonoClass 
	"Returns the namespace of aMonoClass.

		const char* mono_class_get_namespace (MonoClass *klass);"

	<stdcall: char* mono_class_get_namespace MonoClass*>
	self invalidCall!

monoClassGetNestingType: aMonoClass 
	"Returns the outer class of aMonoClass.

		MonoClass* mono_class_get_nesting_type(MonoClass *klass);"

	<stdcall: MonoClass* mono_class_get_nesting_type MonoClass*>
	self invalidCall!

monoClassGetParent: aMonoClass 
	"Returns the parent class of aMonoClass.

		MonoClass* mono_class_get_parent(MonoClass *klass);"

	<stdcall: MonoClass* mono_class_get_parent MonoClass*>
	self invalidCall!

monoClassGetProperties: aMonoClass iter: iter 
	"

		MonoProperty* mono_class_get_properties(
			MonoClass* klass,
			void **iter
		);"

	<stdcall: MonoProperty* mono_class_get_properties MonoClass* void**>
	self invalidCall!

monoClassGetRank: aMonoClass 
	"Returns the rank for aMonoClass array (the number of dimensions).

		int mono_class_get_rank(MonoClass *klass);"

	<stdcall: sdword mono_class_get_rank MonoClass*>
	self invalidCall!

monoClassGetType: aMonoClass 
	"Returns the internal Type representation for aMonoClass.

		MonoType* mono_class_get_type(MonoClass *klass);"

	<stdcall: MonoType* mono_class_get_type MonoClass*>
	self invalidCall!

monoClassInit: aMonoClass 
	"Initialize aMonoClass: compute the instance_size, class_size and other infos that cannot
	be computed at mono_class_get() time.

		mono_bool mono_class_init(MonoClass *klass);"

	<stdcall: bool mono_class_init MonoClass*>
	self invalidCall!

monoClassInstanceSize: aMonoClass 
	"Returns the size of an object instance.

		int32_t mono_class_instance_size(MonoClass *klass);"

	<stdcall: sdword mono_class_instance_size MonoClass*>
	self invalidCall!

monoClassIsEnum: aMonoClass 
	"Returns true if aMonoClass represents an enumeration.

		mono_bool mono_class_is_enum(MonoClass *klass);"

	<stdcall: bool mono_class_is_enum MonoClass*>
	self invalidCall!

monoClassIsValueType: aMonoClass 
	"Returns true if aMonoClass represents a ValueType.

		mono_bool mono_class_is_valuetype(MonoClass *klass);"

	<stdcall: bool mono_class_is_valuetype MonoClass*>
	self invalidCall!

monoClassNeedsCctorRun: aMonoClass caller: aMonoMethod 
	"Answer whether aMonoClass has a static constructor.

		gboolean mono_class_needs_cctor_run(
			MonoClass *klass,
			MonoMethod *caller
		);"

	<stdcall: bool mono_class_needs_cctor_run MonoClass* MonoMethod*>
	self invalidCall!

monoClassVTable: aMonoDomain klass: aMonoClass 
	"Returns the <MonoVTable> of aMonoClass.

		MonoVTable* mono_class_vtable(
			MonoDomain *domain,
			MonoClass *klass
		);"

	<stdcall: MonoVTable* mono_class_vtable MonoDomain* MonoClass*>
	self invalidCall!

monoConfigParse: filename 
	"Load the Mono configuration file, filename.

		void mono_config_parse (const char *filename);"

	<stdcall: void mono_config_parse char*>
	self invalidCall!

monoDomainAssemblyOpen: aMonoDomain name: name 
	"Opens the PE-image pointed by name, and loads any external assemblies referenced by it
	into the specified domain.

		MonoAssembly* mono_domain_assembly_open(
			MonoDomain *domain,
			const char *name
		);"

	<stdcall: MonoAssembly* mono_domain_assembly_open MonoDomain* char*>
	self invalidCall!

monoDomainGet
	"Returns the current domain.

		MonoDomain* mono_domain_get();"

	<stdcall: MonoDomain* mono_domain_get>
	self invalidCall!

monoEnvironmentExitcodeGet
	"

		int32_t mono_environment_exitcode_get (void);"

	<stdcall: sdword mono_environment_exitcode_get>
	self invalidCall!

monoFieldGetFlags: aMonoClassField 
	"

		uint32_t mono_field_get_flags(MonoClassField *field);"

	<stdcall: dword mono_field_get_flags MonoClassField*>
	self invalidCall!

monoFieldGetName: aMonoClassField 
	"Returns the name of aMonoClassField.

		const char* mono_field_get_name(MonoClassField *field);"

	<stdcall: char* mono_field_get_name MonoClassField*>
	self invalidCall!

monoFieldGetParent: aMonoClassField 
	"Returns the <MonoClass> where aMonoClassField was defined.

		MonoClass* mono_field_get_parent(MonoClassField *field);"

	<stdcall: MonoClass* mono_field_get_parent MonoClassField*>
	self invalidCall!

monoFieldGetType: aMonoClassField 
	"Returns the <MonoType> of aMonoClassField.

		MonoType* mono_field_get_type(MonoClassField *field);"

	<stdcall: MonoType* mono_field_get_type MonoClassField*>
	self invalidCall!

monoFieldGetValueObject: aMonoDomain field: aMonoClassField obj: aMonoObject 
	"Returns a new <MonoObject> with the value from aMonoClassField.

		MonoObject *mono_field_get_value_object(
			MonoDomain *domain,
			MonoClassField *field,
			MonoObject *obj
		);"

	<stdcall: MonoObject* mono_field_get_value_object MonoDomain* MonoClassField* MonoObject*>
	self invalidCall!

monoFieldSetValue: aMonoObject field: aMonoClassField value: anExternalAddress 
	"Sets the value of aMonoClassField in aMonoObject.

		void mono_field_set_value(
			MonoObject *obj,
			MonoClassField *field,
			void *value
		);"

	<stdcall: void mono_field_set_value MonoObject* MonoClassField* void*>
	self invalidCall!

monoFieldStaticSetValue: aMonoVTable field: aMonoClassField value: anExternalAddress 
	"Sets the value of the static aMonoClassField.

		void mono_field_static_set_value(
			MonoVTable *vt,
			MonoClassField *field,
			void *value
		);"

	<stdcall: void mono_field_static_set_value MonoVTable* MonoClassField* void*>
	self invalidCall!

monoFree: anExternalAddress 
	"Frees anExternalAddress.

		void mono_free(void *);"

	<stdcall: void mono_free void*>
	self invalidCall!

monoGCCollect: anInteger 
	"

		void mono_gc_collect(int generation);"

	<stdcall: void mono_gc_collect sdword>
	self invalidCall!

monoGCGetGeneration: aMonoObject 
	"

		int mono_gc_get_generation(MonoObject *object);"

	<stdcall: sdword mono_gc_get_generation MonoObject*>
	self invalidCall!

monoGCGetHeapSize
	"

		int64_t mono_gc_get_heap_size(void);"

	<stdcall: sqword mono_gc_get_heap_size>
	self invalidCall!

monoGCGetUsedSize
	"

		int64_t mono_gc_get_used_size(void);"

	<stdcall: sqword mono_gc_get_used_size>
	self invalidCall!

monoGCHandleFree: anInteger 
	"Frees anInteger GC handle.

		void mono_gchandle_free(uint32_t gchandle);"

	<stdcall: void mono_gchandle_free dword>
	self invalidCall!

monoGCHandleNew: aMonoObject pinned: aBoolean 
	"Returns a handle that wraps the object.

		uint32_t mono_gchandle_new(
			MonoObject *obj,
			mono_bool pinned
		);"

	<stdcall: dword mono_gchandle_new MonoObject* bool>
	self invalidCall!

monoGCMaxGeneration
	"

		int mono_gc_max_generation(void);"

	<stdcall: sdword mono_gc_max_generation>
	self invalidCall!

monoGCWBarrierSetArrayRef: aMonoArray slotPtr: anExternalAddress value: aMonoObject 
	"

		void mono_gc_wbarrier_set_arrayref(
			MonoArray *arr,
			void* slot_ptr,
			MonoObject* value
		);"

	<stdcall: void mono_gc_wbarrier_set_arrayref MonoArray* void* MonoObject*>
	self invalidCall!

monoGetArrayClass
	"Returns the <MonoClass> handle for the System.Array built-in CLI type.

		MonoClass* mono_get_array_class(void);"

	<stdcall: MonoClass* mono_get_array_class>
	self invalidCall!

monoGetBooleanClass
	"Returns the <MonoClass> handle for the 'bool' (System.Boolean) built-in CLI type.

		MonoClass* mono_get_boolean_class(void);"

	<stdcall: MonoClass* mono_get_boolean_class>
	self invalidCall!

monoGetByteClass
	"Returns the <MonoClass> handle for the 'byte' (System.Byte) built-in CLI type.

		MonoClass* mono_get_byte_class(void);"

	<stdcall: MonoClass* mono_get_byte_class>
	self invalidCall!

monoGetCharClass
	"Returns the <MonoClass> handle for the 'char' (System.Char) built-in CLI type.

		MonoClass* mono_get_char_class(void);"

	<stdcall: MonoClass* mono_get_char_class>
	self invalidCall!

monoGetCorlib
	"

		MonoImage* mono_get_corlib();"

	<stdcall: MonoImage* mono_get_corlib>
	self invalidCall!

monoGetDoubleClass
	"Returns the <MonoClass> handle for the 'double' (System.Double) built-in CLI type.

		MonoClass* mono_get_double_class(void);"

	<stdcall: MonoClass* mono_get_double_class>
	self invalidCall!

monoGetEnumClass
	"Returns the <MonoClass> handle for the 'enum' (System.Enum) built-in CLI type.

		MonoClass* mono_get_enum_class(void);"

	<stdcall: MonoClass* mono_get_enum_class>
	self invalidCall!

monoGetExceptionClass
	"Returns the <MonoClass> handle for the System.Exception built-in CLI type.

		MonoClass* mono_get_exception_class(void);"

	<stdcall: MonoClass* mono_get_exception_class>
	self invalidCall!

monoGetInt16Class
	"Returns the <MonoClass> handle for the 'short' (System.Int16) built-in CLI type.

		MonoClass* mono_get_int16_class(void);"

	<stdcall: MonoClass* mono_get_int16_class>
	self invalidCall!

monoGetInt32Class
	"Returns the <MonoClass> handle for the 'int' (System.Int32) built-in CLI type.

		MonoClass* mono_get_int32_class(void);"

	<stdcall: MonoClass* mono_get_int32_class>
	self invalidCall!

monoGetInt64Class
	"Returns the <MonoClass> handle for the 'long' (System.Int64) built-in CLI type.

		MonoClass* mono_get_int64_class(void);"

	<stdcall: MonoClass* mono_get_int64_class>
	self invalidCall!

monoGetIntPtrClass
	"Returns the <MonoClass> handle for the System.IntPtr built-in CLI type.

		MonoClass* mono_get_intptr_class(void);"

	<stdcall: MonoClass* mono_get_intptr_class>
	self invalidCall!

monoGetObjectClass
	"Returns the <MonoClass> handle for the 'object' (System.Object) built-in CLI type.

		MonoClass* mono_get_object_class(void);"

	<stdcall: MonoClass* mono_get_object_class>
	self invalidCall!

monoGetSByteClass
	"Returns the <MonoClass> handle for the 'sbyte' (System.SByte) built-in CLI type.

		MonoClass* mono_get_sbyte_class(void);"

	<stdcall: MonoClass* mono_get_sbyte_class>
	self invalidCall!

monoGetSingleClass
	"Returns the <MonoClass> handle for the 'float' (System.Single) built-in CLI type.

		MonoClass* mono_get_single_class(void);"

	<stdcall: MonoClass* mono_get_single_class>
	self invalidCall!

monoGetStringClass
	"Returns the <MonoClass> handle for the 'string' (System.String) built-in CLI type.

		MonoClass* mono_get_string_class(void);"

	<stdcall: MonoClass* mono_get_string_class>
	self invalidCall!

monoGetThreadClass
	"Returns the <MonoClass> handle for the System.Threading.Thread built-in CLI type.

		MonoClass* mono_get_thread_class(void);"

	<stdcall: MonoClass* mono_get_thread_class>
	self invalidCall!

monoGetUInt32Class
	"Returns the <MonoClass> handle for the 'uint' (System.UInt32) built-in CLI type.

		MonoClass* mono_get_uint32_class(void);"

	<stdcall: MonoClass* mono_get_uint32_class>
	self invalidCall!

monoGetUInt64Class
	"Returns the <MonoClass> handle for the 'ulong' (System.UInt64) built-in CLI type.

		MonoClass* mono_get_uint64_class(void);"

	<stdcall: MonoClass* mono_get_uint64_class>
	self invalidCall!

monoGetUIntPtrClass
	"Returns the <MonoClass> handle for the System.UIntPtr built-in CLI type.

		MonoClass* mono_get_uintptr_class(void);"

	<stdcall: MonoClass* mono_get_uintptr_class>
	self invalidCall!

monoGetVoidClass
	"Returns the <MonoClass> handle for the 'void' (System.Void) built-in CLI type.

		MonoClass* mono_get_void_class(void);"

	<stdcall: MonoClass* mono_get_void_class>
	self invalidCall!

monoImageGetFilename: aMonoImage 
	"Used to get the filename that hold the actual MonoImage.

		const char* mono_image_get_filename(MonoImage *image);"

	<stdcall: char* mono_image_get_filename MonoImage*>
	self invalidCall!

monoImageGetName: aMonoImage 
	"Returns the name of the assembly.

		const char* mono_image_get_name(MonoImage *image);"

	<stdcall: char* mono_image_get_name MonoImage*>
	self invalidCall!

monoImageGetTableRows: aMonoImage tableId: anInteger 
	"

		int mono_image_get_table_rows(
			MonoImage *image,
			int table_id
		);"

	<stdcall: sdword mono_image_get_table_rows MonoImage* sdword>
	self invalidCall!

monoImageStrError: anInteger 
	"Returns a string describing the error of a recent operation.

		const char* mono_image_strerror(MonoImageOpenStatus status);"

	<stdcall: char* mono_image_strerror sdword>
	self invalidCall!

monoInstallAssemblyPreloadHook: anExternalCallback userData: userData 
	"Return the MonoImage associated with the specified assembly.

		void mono_install_assembly_preload_hook(
			MonoAssemblyPreLoadFunc func,
			void* user_data
		);"

	<stdcall: void mono_install_assembly_preload_hook lpvoid void*>
	self invalidCall!

monoJITCleanup: aMonoDomain 
	"Shutdown the Mono runtime.

		void mono_jit_cleanup(MonoDomain *domain);"

	<stdcall: void mono_jit_cleanup MonoDomain*>
	self invalidCall!

monoJITExec: aMonoDomain assembly: aMonoAssembly argc: argc argv: argv 
	"Start execution of a program.

		int mono_jit_exec(
			MonoDomain *domain,
			MonoAssembly *assembly,
			int argc,
			char *argv[]
		);"

	<stdcall: sdword mono_jit_exec MonoDomain* MonoAssembly* sdword char**>
	self invalidCall!

monoJITInit: file 
	"Initialize the JIT runtime.

		MonoDomain* mono_jit_init(const char *file);"

	<stdcall: MonoDomain* mono_jit_init char*>
	self invalidCall!

monoJITInitVersion: aDomainNameString runtimeVersion: aVersionString 
	"Initialize the specified framework version.

		MonoDomain* mono_jit_init_version(
			const char *root_domain_name,
			const char *runtime_version
		);"

	<stdcall: MonoDomain* mono_jit_init_version char* char*>
	self invalidCall!

monoMetadataSignatureEqual: aMonoMethodSignature1 sig2: aMonoMethodSignature2 
	"Answer whether two <MonoMethodSignature>s are equal.

		mono_bool mono_metadata_signature_equal(
			MonoMethodSignature *sig1,
			MonoMethodSignature *sig2
		);"

	<stdcall: bool mono_metadata_signature_equal MonoMethodSignature* MonoMethodSignature*>
	self invalidCall!

monoMetadataTypeEqual: aMonoType1 t2: aMonoType2 
	"Answer whether two <MonoType>s are equal.

		mono_bool mono_metadata_type_equal(
			MonoType *t1,
			MonoType *t2
		);"

	<stdcall: bool mono_metadata_type_equal MonoType* MonoType*>
	self invalidCall!

monoMetadataTypeHash: aMonoType 
	"Returns the hash for aMonoType.

		unsigned int mono_metadata_type_hash(MonoType *t1);"

	<stdcall: dword mono_metadata_type_hash MonoType*>
	self invalidCall!

monoMethodDescFree: aMonoMethodDesc 
	"Releases the aMonoMethodDesc object.

		void mono_method_desc_free(MonoMethodDesc *desc);"

	<stdcall: void mono_method_desc_free MonoMethodDesc*>
	self invalidCall!

monoMethodDescNew: aString includeNamespace: aBoolean 
	"Returns a parsed representation of the method description.

		MonoMethodDesc* mono_method_desc_new(
			const char *name,
			mono_bool include_namespace
		);"

	<stdcall: MonoMethodDesc* mono_method_desc_new char* bool>
	self invalidCall!

monoMethodDescSearchInClass: aMonoMethodDesc klass: aMonoClass 
	"

		MonoMethod* mono_method_desc_search_in_class(
			MonoMethodDesc *desc,
			MonoClass *klass
		);"

	<stdcall: MonoMethod* mono_method_desc_search_in_class MonoMethodDesc* MonoClass*>
	self invalidCall!

monoMethodFullName: aMonoMethod signature: aBoolean 
	"Answer the full name of aMonoMethod.

		char* mono_method_full_name(
			MonoMethod *method,
			mono_bool signature
		);"

	<stdcall: char* mono_method_full_name MonoMethod* bool>
	self invalidCall!

monoMethodGetClass: aMonoMethod 
	"

		MonoClass* mono_method_get_class(MonoMethod *method);"

	<stdcall: MonoClass* mono_method_get_class MonoMethod*>
	self invalidCall!

monoMethodGetFlags: aMonoMethod iflags: iflags 
	"

		uint32_t mono_method_get_flags(
			MonoMethod *method,
			uint32_t *iflags
		);"

	<stdcall: dword mono_method_get_flags MonoMethod* dword*>
	self invalidCall!

monoMethodGetName: aMonoMethod 
	"Answer the name of aMonoMethod.

		const char* mono_method_get_name(MonoMethod *method);"

	<stdcall: char* mono_method_get_name MonoMethod*>
	self invalidCall!

monoMethodGetParamNames: aMonoMethod names: names 
	"

		void mono_method_get_param_names(
			MonoMethod *method,
			const char **names
		);"

	<stdcall: void mono_method_get_param_names MonoMethod* char**>
	self invalidCall!

monoMethodSignature: aMonoMethod 
	"Return the signature of aMonoMethod.

		MonoMethodSignature* mono_method_signature(MonoMethod *method);"

	<stdcall: MonoMethodSignature* mono_method_signature MonoMethod*>
	self invalidCall!

monoObjectClone: aMonoObject 
	"Returns a newly created object who is a shallow copy of aMonoObject.

		MonoObject *mono_object_clone(MonoObject *obj);"

	<stdcall: MonoObject* mono_object_clone MonoObject*>
	self invalidCall!

monoObjectGetClass: aMonoObject 
	"Returns the MonoClass of aMonoObject.

		MonoClass* mono_object_get_class(MonoObject *obj);"

	<stdcall: MonoClass* mono_object_get_class MonoObject*>
	self invalidCall!

monoObjectGetDomain: aMonoObject 
	"Returns the <MonoDomain> where aMonoObject is hosted.

		MonoDomain* mono_object_get_domain (MonoObject *obj);"

	<stdcall: MonoDomain* mono_object_get_domain MonoObject*>
	self invalidCall!

monoObjectHash: aMonoObject 
	"Answer the <Integer> hash value of aMonoObject.

		int mono_object_hash(MonoObject* obj);"

	<stdcall: sdword mono_object_hash MonoObject*>
	self invalidCall!

monoObjectNew: aMonoDomain klass: aMonoClass 
	"Returns a newly created object whose definition is looked up using aMonoClass.

		MonoObject* mono_object_new(
			MonoDomain *domain,
			MonoClass *klass
		);"

	<stdcall: MonoObject* mono_object_new MonoDomain* MonoClass*>
	self invalidCall!

monoObjectUnbox: aMonoObject 
	"Returns a pointer to the start of the valuetype boxed in this object.

		void* mono_object_unbox(MonoObject *obj);"

	<stdcall: void* mono_object_unbox MonoObject*>
	self invalidCall!

monoPropertyGetGetMethod: aMonoProperty 
	"Returns the getter method of aMonoProperty.

		MonoMethod* mono_property_get_get_method(MonoProperty *prop);"

	<stdcall: MonoMethod* mono_property_get_get_method MonoProperty*>
	self invalidCall!

monoPropertyGetName: aMonoProperty 
	"Returns the name of aMonoProperty.

		const char* mono_property_get_name(MonoProperty *prop);"

	<stdcall: char* mono_property_get_name MonoProperty*>
	self invalidCall!

monoPropertyGetParent: aMonoProperty 
	"Returns the <MonoClass> where aMonoProperty was defined.

		MonoClass* mono_property_get_parent(MonoProperty *prop);"

	<stdcall: MonoClass* mono_property_get_parent MonoProperty*>
	self invalidCall!

monoPropertyGetSetMethod: aMonoProperty 
	"Returns the setter method of aMonoProperty.

		MonoMethod* mono_property_get_set_method(MonoProperty *prop);"

	<stdcall: MonoMethod* mono_property_get_set_method MonoProperty*>
	self invalidCall!

monoReflectionTypeFromName: aString image: aMonoImage 
	"

		MonoType* mono_reflection_type_from_name(
			char *name,
			MonoImage *image
		);"

	<stdcall: MonoType* mono_reflection_type_from_name char* MonoImage*>
	self invalidCall!

monoRuntimeClassInit: aMonoVTable 
	"Calls the class constructor for aMonoVTable.

		void mono_runtime_class_init(MonoVTable *vtable);"

	<stdcall: void mono_runtime_class_init MonoVTable*>
	self invalidCall!

monoRuntimeInvoke: aMonoMethod obj: obj params: params exc: exc 
	"Invokes the method represented by aMonoMethod on the object obj.

		MonoObject* mono_runtime_invoke(
			MonoMethod *method,
			void *obj,
			void **params,
			MonoObject **exc
		);"

	<stdcall: MonoObject* mono_runtime_invoke MonoMethod* void* void** MonoObject**>
	self invalidCall!

monoRuntimeObjectInit: aMonoObject 
	"Tell the runtime to run the default argumentless constructor in aMonoObject.

		void mono_runtime_object_init(MonoObject *this_obj);"

	<stdcall: void mono_runtime_object_init MonoObject*>
	self invalidCall!

monoSetDirs: assemblyDir configDir: configDir 
	"Inform the Mono runtime where to find its assemblies and configuration files.

		void mono_set_dirs(
			const char *assembly_dir,
			const char *config_dir
		);"

	<stdcall: void mono_set_dirs char* char*>
	self invalidCall!

monoSignatureGetParamCount: aMonoMethodSignature 
	"Return the parameter count of aMonoMethodSignature.

		uint32_t mono_signature_get_param_count(MonoMethodSignature *sig);"

	<stdcall: dword mono_signature_get_param_count MonoMethodSignature*>
	self invalidCall!

monoSignatureGetParams: aMonoMethodSignature iter: iter 
	"

		MonoType* mono_signature_get_params(
			MonoMethodSignature *sig,
			void **iter
		);"

	<stdcall: MonoType* mono_signature_get_params MonoMethodSignature* void**>
	self invalidCall!

monoSignatureGetReturnType: aMonoMethodSignature 
	"

		MonoType* mono_signature_get_return_type(MonoMethodSignature *sig);"

	<stdcall: MonoType* mono_signature_get_return_type MonoMethodSignature*>
	self invalidCall!

monoSignatureHash: aMonoMethodSignature 
	"Returns the hash for aMonoMethodSignature.

		unsigned int mono_signature_hash(MonoMethodSignature *sig);"

	<stdcall: dword mono_signature_hash MonoMethodSignature*>
	self invalidCall!

monoStringEqual: aMonoString1 s2: aMonoString2 
	"Answer whether two <MonoString>s are equal.

		mono_bool mono_string_equal(
			MonoString *s1,
			MonoString *s2
		);"

	<stdcall: bool mono_string_equal MonoString* MonoString*>
	self invalidCall!

monoStringFromUTF16: aUnicodeString 
	"Converts a NULL terminated UTF16 string (LPWSTR) to a MonoString.

		MonoString* mono_string_from_utf16(mono_unichar2 *data);"

	<stdcall: MonoString* mono_string_from_utf16 lpwstr>
	self invalidCall!

monoStringHash: aMonoString 
	"Returns the hash for aMonoString.

		unsigned int mono_string_hash(MonoString *s);"

	<stdcall: dword mono_string_hash MonoString*>
	self invalidCall!

monoStringifyAssemblyName: aMonoAssemblyName 
	"Returns a newly allocated string with a string representation of the assembly name.

	Implementation Note: Changed the function's return value to 'void*' so we can free the pointer.

		char* mono_stringify_assembly_name(MonoAssemblyName *aname);"

	<stdcall: void* mono_stringify_assembly_name MonoAssemblyName*>
	self invalidCall!

monoStringLength: aMonoString 
	"Returns the lenght in characters of aMonoString.

		int mono_string_length(MonoString *s);"

	<stdcall: sdword mono_string_length MonoString*>
	self invalidCall!

monoStringNew: aMonoDomain text: aString 
	"Returns a MonoString object which contains aString.

		MonoString* mono_string_new(
			MonoDomain *domain,
			const char *text
		);"

	<stdcall: MonoString* mono_string_new MonoDomain* char*>
	self invalidCall!

monoStringToUTF16: aMonoString 
	"Return an null-terminated array of the UTF-16 chars contained in aMonoString.

	Implementation Note: The return type is not void*, but we need an <ExternalAddress>.

		mono_unichar2* mono_string_to_utf16 (MonoString *string_obj);"

	<stdcall: void* mono_string_to_utf16 MonoString*>
	self invalidCall!

monoTypeGetName: aMonoType 
	"Returns the string representation for aMonoType as it would be represented in IL code.

		char* mono_type_get_name(MonoType *type);"

	<stdcall: char* mono_type_get_name MonoType*>
	self invalidCall!

monoTypeGetType: aMonoType 
	"Returns the IL type value for aMonoType.

		int mono_type_get_type(MonoType *type);"

	<stdcall: sdword mono_type_get_type MonoType*>
	self invalidCall!

monoTypeIsByRef: aMonoType 
	"Answer whether aMonoType represents a type passed by reference.

		mono_bool mono_type_is_byref(MonoType *type);"

	<stdcall: bool mono_type_is_byref MonoType*>
	self invalidCall!

monoTypeIsReference: aMonoType 
	"Answer whether aMonoType is a reference type.

		mono_bool mono_type_is_reference (MonoType *type);"

	<stdcall: bool mono_type_is_reference MonoType*>
	self invalidCall!

monoValueBox: domain klass: aMonoClass val: anExternalAddress 
	"

		MonoObject* mono_value_box(
			MonoDomain *domain,
			MonoClass *klass,
			void* val
		);"

	<stdcall: MonoObject* mono_value_box MonoDomain* MonoClass* void*>
	self invalidCall! !
!MonoLibrary categoriesFor: #monoArrayAddrWithSize:size:idx:!objects!public! !
!MonoLibrary categoriesFor: #monoArrayClone:!objects!public! !
!MonoLibrary categoriesFor: #monoArrayElementSize:!objects!public! !
!MonoLibrary categoriesFor: #monoArrayLength:!objects!public! !
!MonoLibrary categoriesFor: #monoArrayNew:eclass:n:!objects!public! !
!MonoLibrary categoriesFor: #monoAssemblyGetImage:!assemblies!public! !
!MonoLibrary categoriesFor: #monoAssemblyLoad:basedir:status:!assemblies!public! !
!MonoLibrary categoriesFor: #monoAssemblyLoaded:!assemblies!public! !
!MonoLibrary categoriesFor: #monoAssemblyNameFree:!assemblies!public! !
!MonoLibrary categoriesFor: #monoAssemblyNameGetName:!assemblies!public! !
!MonoLibrary categoriesFor: #monoAssemblyNameNew:!assemblies!public! !
!MonoLibrary categoriesFor: #monoAssemblyNamesEqual:r:!assemblies!public! !
!MonoLibrary categoriesFor: #monoAssemblyOpen:status:!assemblies!public! !
!MonoLibrary categoriesFor: #monoClassFromMonoType:!classes!public! !
!MonoLibrary categoriesFor: #monoClassFromName:namespace:name:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGet:typeToken:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetCctor:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetElementClass:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetFieldFromName:name:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetFields:iter:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetFlags:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetImage:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetInterfaces:iter:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetMethods:iter:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetName:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetNamespace:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetNestingType:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetParent:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetProperties:iter:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetRank:!classes!public! !
!MonoLibrary categoriesFor: #monoClassGetType:!classes!public! !
!MonoLibrary categoriesFor: #monoClassInit:!classes!public! !
!MonoLibrary categoriesFor: #monoClassInstanceSize:!classes!public! !
!MonoLibrary categoriesFor: #monoClassIsEnum:!classes!public! !
!MonoLibrary categoriesFor: #monoClassIsValueType:!classes!public! !
!MonoLibrary categoriesFor: #monoClassNeedsCctorRun:caller:!classes!public! !
!MonoLibrary categoriesFor: #monoClassVTable:klass:!classes!public! !
!MonoLibrary categoriesFor: #monoConfigParse:!embedding!public! !
!MonoLibrary categoriesFor: #monoDomainAssemblyOpen:name:!application domains!public! !
!MonoLibrary categoriesFor: #monoDomainGet!application domains!public! !
!MonoLibrary categoriesFor: #monoEnvironmentExitcodeGet!public! !
!MonoLibrary categoriesFor: #monoFieldGetFlags:!objects!public! !
!MonoLibrary categoriesFor: #monoFieldGetName:!objects!public! !
!MonoLibrary categoriesFor: #monoFieldGetParent:!objects!public! !
!MonoLibrary categoriesFor: #monoFieldGetType:!objects!public! !
!MonoLibrary categoriesFor: #monoFieldGetValueObject:field:obj:!objects!public! !
!MonoLibrary categoriesFor: #monoFieldSetValue:field:value:!objects!public! !
!MonoLibrary categoriesFor: #monoFieldStaticSetValue:field:value:!objects!public! !
!MonoLibrary categoriesFor: #monoFree:!public! !
!MonoLibrary categoriesFor: #monoGCCollect:!garbage collection!public! !
!MonoLibrary categoriesFor: #monoGCGetGeneration:!garbage collection!public! !
!MonoLibrary categoriesFor: #monoGCGetHeapSize!garbage collection!public! !
!MonoLibrary categoriesFor: #monoGCGetUsedSize!garbage collection!public! !
!MonoLibrary categoriesFor: #monoGCHandleFree:!gc handles!public! !
!MonoLibrary categoriesFor: #monoGCHandleNew:pinned:!gc handles!public! !
!MonoLibrary categoriesFor: #monoGCMaxGeneration!garbage collection!public! !
!MonoLibrary categoriesFor: #monoGCWBarrierSetArrayRef:slotPtr:value:!objects!public! !
!MonoLibrary categoriesFor: #monoGetArrayClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetBooleanClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetByteClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetCharClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetCorlib!public! !
!MonoLibrary categoriesFor: #monoGetDoubleClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetEnumClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetExceptionClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetInt16Class!common types!public! !
!MonoLibrary categoriesFor: #monoGetInt32Class!common types!public! !
!MonoLibrary categoriesFor: #monoGetInt64Class!common types!public! !
!MonoLibrary categoriesFor: #monoGetIntPtrClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetObjectClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetSByteClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetSingleClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetStringClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetThreadClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetUInt32Class!common types!public! !
!MonoLibrary categoriesFor: #monoGetUInt64Class!common types!public! !
!MonoLibrary categoriesFor: #monoGetUIntPtrClass!common types!public! !
!MonoLibrary categoriesFor: #monoGetVoidClass!common types!public! !
!MonoLibrary categoriesFor: #monoImageGetFilename:!images!public! !
!MonoLibrary categoriesFor: #monoImageGetName:!images!public! !
!MonoLibrary categoriesFor: #monoImageGetTableRows:tableId:!images!public! !
!MonoLibrary categoriesFor: #monoImageStrError:!images!public! !
!MonoLibrary categoriesFor: #monoInstallAssemblyPreloadHook:userData:!assemblies!public! !
!MonoLibrary categoriesFor: #monoJITCleanup:!embedding!public! !
!MonoLibrary categoriesFor: #monoJITExec:assembly:argc:argv:!public! !
!MonoLibrary categoriesFor: #monoJITInit:!embedding!public! !
!MonoLibrary categoriesFor: #monoJITInitVersion:runtimeVersion:!embedding!public! !
!MonoLibrary categoriesFor: #monoMetadataSignatureEqual:sig2:!metadata access!public! !
!MonoLibrary categoriesFor: #monoMetadataTypeEqual:t2:!metadata access!public! !
!MonoLibrary categoriesFor: #monoMetadataTypeHash:!metadata access!public! !
!MonoLibrary categoriesFor: #monoMethodDescFree:!methods!public! !
!MonoLibrary categoriesFor: #monoMethodDescNew:includeNamespace:!methods!public! !
!MonoLibrary categoriesFor: #monoMethodDescSearchInClass:klass:!methods!public! !
!MonoLibrary categoriesFor: #monoMethodFullName:signature:!methods!public! !
!MonoLibrary categoriesFor: #monoMethodGetClass:!methods!public! !
!MonoLibrary categoriesFor: #monoMethodGetFlags:iflags:!methods!public! !
!MonoLibrary categoriesFor: #monoMethodGetName:!methods!public! !
!MonoLibrary categoriesFor: #monoMethodGetParamNames:names:!methods!public! !
!MonoLibrary categoriesFor: #monoMethodSignature:!methods!public! !
!MonoLibrary categoriesFor: #monoObjectClone:!objects!public! !
!MonoLibrary categoriesFor: #monoObjectGetClass:!objects!public! !
!MonoLibrary categoriesFor: #monoObjectGetDomain:!objects!public! !
!MonoLibrary categoriesFor: #monoObjectHash:!objects!public! !
!MonoLibrary categoriesFor: #monoObjectNew:klass:!objects!public! !
!MonoLibrary categoriesFor: #monoObjectUnbox:!objects!public! !
!MonoLibrary categoriesFor: #monoPropertyGetGetMethod:!objects!public! !
!MonoLibrary categoriesFor: #monoPropertyGetName:!objects!public! !
!MonoLibrary categoriesFor: #monoPropertyGetParent:!objects!public! !
!MonoLibrary categoriesFor: #monoPropertyGetSetMethod:!objects!public! !
!MonoLibrary categoriesFor: #monoReflectionTypeFromName:image:!public!types! !
!MonoLibrary categoriesFor: #monoRuntimeClassInit:!classes!public! !
!MonoLibrary categoriesFor: #monoRuntimeInvoke:obj:params:exc:!methods!public! !
!MonoLibrary categoriesFor: #monoRuntimeObjectInit:!public! !
!MonoLibrary categoriesFor: #monoSetDirs:configDir:!embedding!public! !
!MonoLibrary categoriesFor: #monoSignatureGetParamCount:!methods!public! !
!MonoLibrary categoriesFor: #monoSignatureGetParams:iter:!methods!public! !
!MonoLibrary categoriesFor: #monoSignatureGetReturnType:!methods!public! !
!MonoLibrary categoriesFor: #monoSignatureHash:!methods!public! !
!MonoLibrary categoriesFor: #monoStringEqual:s2:!public!strings! !
!MonoLibrary categoriesFor: #monoStringFromUTF16:!public!strings! !
!MonoLibrary categoriesFor: #monoStringHash:!public!strings! !
!MonoLibrary categoriesFor: #monoStringifyAssemblyName:!assemblies!public! !
!MonoLibrary categoriesFor: #monoStringLength:!public!strings! !
!MonoLibrary categoriesFor: #monoStringNew:text:!public!strings! !
!MonoLibrary categoriesFor: #monoStringToUTF16:!public!strings! !
!MonoLibrary categoriesFor: #monoTypeGetName:!public!types! !
!MonoLibrary categoriesFor: #monoTypeGetType:!public!types! !
!MonoLibrary categoriesFor: #monoTypeIsByRef:!public!types! !
!MonoLibrary categoriesFor: #monoTypeIsReference:!public!types! !
!MonoLibrary categoriesFor: #monoValueBox:klass:val:!objects!public! !

!MonoLibrary class methodsFor!

fileName
	"Answer the host system file name for the external library the receiver represents."

	^(Folder pathname: MonoRuntime current assemblyPath) parent pathname , 'bin\mono-2.0.dll'! !
!MonoLibrary class categoriesFor: #fileName!constants!public! !

BOOLEAN guid: (GUID fromString: '{3568E346-ABC7-4571-B2A4-B10198FCD6FB}')!
BOOLEAN comment: ''!
!BOOLEAN categoriesForClass!External-Data-Structured! !
!BOOLEAN methodsFor!

value
	"Answer the receiver's value field as a Smalltalk object."

	^(bytes sbyteAtOffset: 0) asBoolean!

value: anObject 
	"Set the receiver's value field to the value of anObject."

	bytes sbyteAtOffset: 0 put: anObject asParameter! !
!BOOLEAN categoriesFor: #value!accessing!public! !
!BOOLEAN categoriesFor: #value:!accessing!public! !

!BOOLEAN class methodsFor!

typeName
	"Private - Answer the Dolphin external type name for the receiver.
	There is an appropriate built-in type which we can substitute."

	^#boolean! !
!BOOLEAN class categoriesFor: #typeName!constants!private! !

MonoAssembly guid: (GUID fromString: '{B06BC499-66D5-49AD-AAFF-24C313B9B5BE}')!
MonoAssembly comment: ''!
!MonoAssembly categoriesForClass!External-Data-Unstructured! !
!MonoAssembly methodsFor!

doesNotUnderstand: aMessage 
	"Private - Sent to the receiver by the VM when a message sent to the receiver was 
	not implemented by the receiver or its superclasses. In this case then aMessage
	is forwarded to the receiver's image."

	^aMessage forwardTo: self image!

image
	"Answer the <MonoImage> associated with the receiver."

	^MonoLibrary default monoAssemblyGetImage: self!

run
	"Run the Main() method in the receiver."

	self runWithArguments: #()!

runWithArguments: aSequenceableCollection 
	"Run the Main() method in the receiver passing it the arguments in aSequenceableCollection."

	| arguments argc argv |
	arguments := (Array with: self fileName) , aSequenceableCollection.
	argc := arguments size.
	argv := PointerArray length: argc elementClass: String.
	1 to: argc do: [:index | argv at: index put: (arguments at: index)].
	^MonoLibrary default 
		monoJITExec: MonoDomain current
		assembly: self
		argc: argc
		argv: argv! !
!MonoAssembly categoriesFor: #doesNotUnderstand:!exceptions!private! !
!MonoAssembly categoriesFor: #image!public! !
!MonoAssembly categoriesFor: #run!public! !
!MonoAssembly categoriesFor: #runWithArguments:!public! !

!MonoAssembly class methodsFor!

named: aString 
	"Answer a new instance of the receiver named aString."

	^(MonoAssemblyName name: aString) monoAssembly! !
!MonoAssembly class categoriesFor: #named:!public! !

MonoAssemblyName guid: (GUID fromString: '{7BF6CF80-B7A8-46AA-B676-1EA230385894}')!
MonoAssemblyName comment: ''!
!MonoAssemblyName categoriesForClass!External-Data-Unstructured! !
!MonoAssemblyName methodsFor!

= comparand 
	"Answer whether the receiver and the <MonoAssemblyName>, comparand, are considered
	equivalent."

	^self species == comparand species and: [MonoLibrary default monoAssemblyNamesEqual: self r: comparand]!

asString
	"Answer the string representation for the receiver."

	| externalAddress string |
	externalAddress := MonoLibrary default monoStringifyAssemblyName: self.
	string := String fromAddress: externalAddress.
	MonoLibrary default monoFree: externalAddress.
	^string!

basicFree
	"Private - Free external resources held by the receiver."

	MonoLibrary default monoAssemblyNameFree: self.
	MonoLibrary default monoFree: self!

hash
	"Answer the <Integer> hash value for the receiver."

	^self asString hash!

isLoaded
	"Answer whether the <MonoAssembly> referenced by the receiver is loaded."

	^self monoAssemblyLoaded notNull!

load
	"Load the <MonoAssembly> referenced by the receiver."

	^self loadFromPath: nil!

loadFromPath: aPathString 
	"Load the <MonoAssembly> referenced by the receiver from aPathString."

	| status assembly |
	status := SDWORD new.
	assembly := MonoLibrary default 
				monoAssemblyLoad: self
				basedir: aPathString
				status: status.
	^status ~= MONO_IMAGE_OK 
		ifTrue: 
			[| errorText |
			errorText := MonoImage errorTextFor: status value.
			self error: 'Can''t find assembly named ' , '''' , self name , ''': ' , errorText]
		ifFalse: [assembly]!

monoAssembly
	"Answer the <MonoAssembly> the receiver describes."

	| assembly |
	assembly := self monoAssemblyLoaded.
	^assembly isNull ifTrue: [self load] ifFalse: [assembly]!

monoAssemblyLoaded
	"Private - Answer the <MonoAssembly> referenced by the receiver."

	^MonoLibrary default monoAssemblyLoaded: self!

name
	"Answer the receiver's name."

	^MonoLibrary default monoAssemblyNameGetName: self!

needsFree
	"Private - Answer whether the receiver requires freeing of any external resources."

	^self notNull! !
!MonoAssemblyName categoriesFor: #=!comparing!public! !
!MonoAssemblyName categoriesFor: #asString!converting!public! !
!MonoAssemblyName categoriesFor: #basicFree!private!realizing/unrealizing! !
!MonoAssemblyName categoriesFor: #hash!comparing!public! !
!MonoAssemblyName categoriesFor: #isLoaded!public!testing! !
!MonoAssemblyName categoriesFor: #load!public! !
!MonoAssemblyName categoriesFor: #loadFromPath:!public! !
!MonoAssemblyName categoriesFor: #monoAssembly!converting!public! !
!MonoAssemblyName categoriesFor: #monoAssemblyLoaded!private! !
!MonoAssemblyName categoriesFor: #name!public! !
!MonoAssemblyName categoriesFor: #needsFree!private!realizing/unrealizing! !

!MonoAssemblyName class methodsFor!

name: aString 
	"Answer a new instance of the receiver with aString name."

	^(MonoLibrary default monoAssemblyNameNew: aString)
		beFinalizable;
		yourself!

new
	"Should not implement. Use #named:"

	^self shouldNotImplement! !
!MonoAssemblyName class categoriesFor: #name:!instance creation!public! !
!MonoAssemblyName class categoriesFor: #new!instance creation!public! !

MonoClass guid: (GUID fromString: '{B81280F6-444A-47F2-AD84-10D957D9548B}')!
MonoClass comment: ''!
!MonoClass categoriesForClass!External-Data-Unstructured! !
!MonoClass methodsFor!

attributes
	"Private - Answer the receiver's attributes."

	^MonoLibrary default monoClassGetFlags: self!

classSemanticsMask
	"Private - Specifies class semantics information; the current class is contextful (else agile)."

	^self attributes bitAnd: MONO_TYPE_ATTR_CLASS_SEMANTIC_MASK!

customFormatMask
	"Private - Used to retrieve non-standard encoding information for native interop."

	^self attributes bitAnd: MONO_TYPE_ATTR_CUSTOM_MASK!

elementClass
	"Answer the receiver's element <MonoClass>."

	^MonoLibrary default monoClassGetElementClass: self!

elementSize
	"Answer the size in bytes of the indidividual elements of the receiver."

	^MonoLibrary default monoArrayElementSize: self!

fieldNamed: aString 
	"Answer the <MonoClassField> named aString in the receiver or one of its superclasses."

	^MonoLibrary default monoClassGetFieldFromName: self name: aString!

fields
	"Answer a collection of the receiver's fields."

	| iterator field fields |
	iterator := LPVOID new.
	fields := OrderedCollection new.
	[(field := MonoLibrary default monoClassGetFields: self iter: iterator) isNull] 
		whileFalse: [fields add: field].
	^fields!

fullyQualifiedName
	"Answer the fully qualified name for the receiver."

	| namespace |
	namespace := self namespace.
	^namespace notEmpty ifTrue: [namespace , '.' , self name] ifFalse: [self name]!

hasSecurity
	"Answer whether the receiver has security associated with it."

	^self reservedMask = MONO_TYPE_ATTR_HAS_SECURITY!

hasStaticConstructor
	"Answer whether the receiver has a static constructor method."

	^MonoLibrary default monoClassNeedsCctorRun: self caller: nil!

image
	"Answer the <MonoImage> the receiver belongs to."

	^MonoLibrary default monoClassGetImage: self!

initialize
	"Private - Initialize the receiver."

	"(MonoLibrary default monoClassInit: self) 
		ifFalse: [self error: 'Can''t initialize MonoClass (' , self fullName , ')']"

	(self isBeforeFieldInit and: [self hasStaticConstructor]) ifTrue: [self vTable initialize]!

instanceSize
	"Answer the size of an object instance."

	^MonoLibrary default monoClassInstanceSize: self!

interfaces
	"Answer a collection of the interfaces implemented by the receiver."

	| iterator interface interfaces |
	iterator := LPVOID new.
	interfaces := OrderedCollection new.
	[(interface := MonoLibrary default monoClassGetInterfaces: self iter: iterator) isNull] 
		whileFalse: [interfaces add: interface].
	^interfaces!

isAbstract
	"Answer whether the receiver is abstract."

	^(self attributes bitAnd: MONO_TYPE_ATTR_ABSTRACT) asBoolean!

isAnsiFormat
	"Answer whether LPTSTR is interpreted as ANSI."

	^self stringFormatMask = MONO_TYPE_ATTR_ANSI_CLASS!

isAutoFormat
	"Answer whether LPTSTR is interpreted automatically."

	^self stringFormatMask = MONO_TYPE_ATTR_AUTO_CLASS!

isAutoLayout
	"Answer whether the receiver's fields are automatically laid out by the common language runtime."

	^self layoutMask = MONO_TYPE_ATTR_AUTO_LAYOUT!

isBeforeFieldInit
	"Answer whether calling class ('static' in C# parlance) methods of the receiver does not force
	the system to initialize it."

	^(self attributes bitAnd: MONO_TYPE_ATTR_BEFORE_FIELD_INIT) asBoolean!

isClass
	"Answer whether the receiver is a class."

	^self classSemanticsMask = MONO_TYPE_ATTR_CLASS!

isCompilerGenerated
	"Answer whether the receiver is compiler generated and cannot be referenced."

	^self name includes: $<!

isCustomFormat
	"Answer whether LPSTR is interpreted by some implementation-specific means."

	^self stringFormatMask = MONO_TYPE_ATTR_CUSTOM_CLASS!

isEnumeration
	"Answer whether the receiver represents an enumeration."

	^MonoLibrary default monoClassIsEnum: self!

isExplicitLayout
	"Answer whether the receiver's fields are laid out at the specified offsets."

	^self layoutMask = MONO_TYPE_ATTR_EXPLICIT_LAYOUT!

isForwarder
	^(self attributes bitAnd: MONO_TYPE_ATTR_FORWARDER) asBoolean!

isGeneric
	"Answer whether the receiver is a Generic type."

	^self name includes: $`!

isImported
	"Answer whether the receiver is imported from another module."

	^(self attributes bitAnd: MONO_TYPE_ATTR_IMPORT) asBoolean!

isInterface
	"Answer whether the receiver is an interface."

	^self classSemanticsMask = MONO_TYPE_ATTR_INTERFACE!

isNested
	"Answer whether the receiver is a nested class."

	^self outer notNull!

isNestedAssemblyVisible
	"Answer whether the receiver is nested with assembly visibility, and is thus accessible only
	by methods within its assembly."

	^self visibilityMask = MONO_TYPE_ATTR_NESTED_ASSEMBLY!

isNestedFamilyAndAssemblyVisible
	"Answer whether the receiver is nested with assembly and family visibility, and is thus
	accessible only by methods lying in the intersection of its family and assembly."

	^self visibilityMask = MONO_TYPE_ATTR_NESTED_FAM_AND_ASSEM!

isNestedFamilyOrAssemblyVisible
	"Answer whether the receiver is nested with family or assembly visibility, and is thus
	accessible only by methods lying in the union of its family and assembly."

	^self visibilityMask = MONO_TYPE_ATTR_NESTED_FAM_OR_ASSEM!

isNestedFamilyVisible
	"Answer whether the receiver is nested with family visibility, and is thus accessible only by
	methods within its own type and any subtypes."

	^self visibilityMask = MONO_TYPE_ATTR_NESTED_FAMILY!

isNestedPrivate
	"Answer whether the receiver is nested with private visibility."

	^self visibilityMask = MONO_TYPE_ATTR_NESTED_PRIVATE!

isNestedPublic
	"Answer whether the receiver is nested with public visibility."

	^self visibilityMask = MONO_TYPE_ATTR_NESTED_PUBLIC!

isPublic
	"Answer whether the receiver is public."

	^self visibilityMask = MONO_TYPE_ATTR_PUBLIC!

isRoot
	"Answer whether the receiver is the root of the class hierarchy (aka System.Object)."

	^self superclass isNull!

isRTSpecialName
	"Answer whether the runtime should check name encoding."

	^self reservedMask = MONO_TYPE_ATTR_RT_SPECIAL_NAME!

isSealed
	"Answer whether the receiver is concrete and cannot be extended."

	^(self attributes bitAnd: MONO_TYPE_ATTR_SEALED) asBoolean!

isSequentialLayout
	"Answer whether the receiver's fields are laid out sequentially, in the order that the fields were
	emitted to the metadata."

	^self layoutMask = MONO_TYPE_ATTR_SEQUENTIAL_LAYOUT!

isSerializable
	"Answer whether the receiver can be serialized."

	^(self attributes bitAnd: MONO_TYPE_ATTR_SERIALIZABLE) asBoolean!

isSpecial
	"Answer whether the receiver is special in a way denoted by the name."

	^(self attributes bitAnd: MONO_TYPE_ATTR_SPECIAL_NAME) asBoolean!

isUnicodeFormat
	"Answer whether LPTSTR is interpreted as UNICODE."

	^self stringFormatMask = MONO_TYPE_ATTR_UNICODE_CLASS!

isValueType
	"Answer whether the receiver represents a ValueType."

	^MonoLibrary default monoClassIsValueType: self!

layoutMask
	"Private - Specifies class layout information."

	^self attributes bitAnd: MONO_TYPE_ATTR_LAYOUT_MASK!

methodFor: aMessage 
	"Answer the method for aMessage in the receiver or one of its superclasses.

	Implementation Note: See comment of MonoObject>>doesNotUnderstand:"

	| selectorArray methodName methodArgumentNames methods |
	#todo.	"refactor this method"
	selectorArray := aMessage selector subStrings: $:.
	methodName := selectorArray first asString.
	methodArgumentNames := selectorArray allButFirst.
	methods := self methods select: [:method | method name = methodName].
	^methods isEmpty 
		ifTrue: 
			[| superclass |
			superclass := self superclass.
			superclass isNull ifTrue: [self errorNotFound: aMessage].
			superclass methodFor: aMessage]
		ifFalse: 
			[(methods size == 1 or: [aMessage argumentCount isZero]) 
				ifTrue: [methods first]
				ifFalse: [methods detect: [:method | method argumentNames allButFirst = methodArgumentNames]]]!

methodNamed: aString 
	"Answer the <MonoMethod> named aString in the receiver or one of its superclasses."

	| methodDesc method |
	methodDesc := MonoMethodDesc name: aString.
	method := methodDesc methodIn: self.
	^method isNull ifTrue: [self superclass methodNamed: aString] ifFalse: [method]!

methods
	"Answer a collection of the receiver's methods."

	| iterator method methods |
	iterator := LPVOID new.
	methods := OrderedCollection new.
	[(method := MonoLibrary default monoClassGetMethods: self iter: iterator) isNull] 
		whileFalse: [methods add: method].
	^methods!

monoType
	"Answer the <MonoType> the receiver represents."

	^MonoLibrary default monoClassGetType: self!

name
	"Answer the receiver's name."

	| className |
	className := MonoLibrary default monoClassGetName: self.
	^self isNested ifTrue: [self outer name , '/' , className] ifFalse: [className]!

namespace
	"Answer the receiver's namespace."

	^self isNested 
		ifTrue: [self outer namespace]
		ifFalse: [MonoLibrary default monoClassGetNamespace: self]!

newObject
	"Answer a newly created object whose definition is looked up using the receiver."

	^MonoLibrary default monoObjectNew: MonoDomain current klass: self!

notPublic
	"Answer whether the receiver is not public."

	^self visibilityMask = MONO_TYPE_ATTR_NOT_PUBLIC!

outer
	"Answer the receiver's outer class."

	^MonoLibrary default monoClassGetNestingType: self!

properties
	"Answer a collection of the receiver's properties."

	| iterator property properties |
	iterator := LPVOID new.
	properties := OrderedCollection new.
	[(property := MonoLibrary default monoClassGetProperties: self iter: iterator) isNull] 
		whileFalse: [properties add: property].
	^properties!

rank
	"Answer the receiver's rank (the number of dimensions)."

	^MonoLibrary default monoClassGetRank: self!

reservedMask
	"Private - Attributes reserved for runtime use."

	^self attributes bitAnd: MONO_TYPE_ATTR_RESERVED_MASK!

smalltalkClassName
	"Answer the receiver's Smalltalk class name."

	| stClassName |
	stClassName := (self fullyQualifiedName copyReplacing: $. withObject: $_) copyReplacing: $/ withObject: $_.
	^(self isGeneric ifTrue: [stClassName copyReplacing: $` withObject: $_] ifFalse: [stClassName]) asSymbol!

smalltalkName
	"Answer the receiver's Smalltalk name."

	| stName |
	stName := MonoLibrary default monoClassGetName: self.
	^(self isGeneric ifTrue: [stName copyReplacing: $` withObject: $_] ifFalse: [stName]) asSymbol!

smalltalkSuperclass
	"Answer the receiver's Smalltalk superclass."

	^self isRoot 
		ifFalse: 
			[| superclass |
			superclass := self isNested ifTrue: [self outer] ifFalse: [self superclass].
			Smalltalk at: superclass smalltalkClassName ifAbsent: [MonoObject]]
		ifTrue: [MonoObject]!

smalltalkSuperclassCount
	"Answer the number of superclasses the receiver *should* have in the Smalltalk class hierarchy
	(assuming System_Object being the root class)."

	| class count |
	class := self.
	count := 0.
	[class isRoot] whileFalse: 
			[class := class isNested ifTrue: [class outer] ifFalse: [class superclass].
			count := count + 1].
	^count!

staticConstructor
	"Answer the receiver's static constructor method."

	^MonoLibrary default monoClassGetCctor: self!

stringFormatMask
	"Private - Used to retrieve string information for native interoperability."

	^self attributes bitAnd: MONO_TYPE_ATTR_STRING_FORMAT_MASK!

superclass
	"Answer the receiver's superclass."

	^MonoLibrary default monoClassGetParent: self!

superclasses
	"Answer a collection of the receiver's superclasses."

	| class superclasses |
	class := self.
	superclasses := OrderedCollection new.
	[class isRoot] whileFalse: [class := superclasses add: class superclass].
	^superclasses!

visibilityMask
	"Private - Specifies type visibility information."

	^self attributes bitAnd: MONO_TYPE_ATTR_VISIBILITY_MASK!

vTable
	"Answer the <MonoVTable> of the receiver."

	^MonoLibrary default monoClassVTable: MonoDomain current klass: self! !
!MonoClass categoriesFor: #attributes!private! !
!MonoClass categoriesFor: #classSemanticsMask!private! !
!MonoClass categoriesFor: #customFormatMask!private! !
!MonoClass categoriesFor: #elementClass!accessing!public! !
!MonoClass categoriesFor: #elementSize!accessing!public! !
!MonoClass categoriesFor: #fieldNamed:!public! !
!MonoClass categoriesFor: #fields!public! !
!MonoClass categoriesFor: #fullyQualifiedName!public! !
!MonoClass categoriesFor: #hasSecurity!public!testing! !
!MonoClass categoriesFor: #hasStaticConstructor!public!testing! !
!MonoClass categoriesFor: #image!public! !
!MonoClass categoriesFor: #initialize!initializing!private! !
!MonoClass categoriesFor: #instanceSize!public! !
!MonoClass categoriesFor: #interfaces!public! !
!MonoClass categoriesFor: #isAbstract!public!testing! !
!MonoClass categoriesFor: #isAnsiFormat!public!testing! !
!MonoClass categoriesFor: #isAutoFormat!public!testing! !
!MonoClass categoriesFor: #isAutoLayout!public!testing! !
!MonoClass categoriesFor: #isBeforeFieldInit!public!testing! !
!MonoClass categoriesFor: #isClass!public!testing! !
!MonoClass categoriesFor: #isCompilerGenerated!public!testing! !
!MonoClass categoriesFor: #isCustomFormat!public!testing! !
!MonoClass categoriesFor: #isEnumeration!public!testing! !
!MonoClass categoriesFor: #isExplicitLayout!public!testing! !
!MonoClass categoriesFor: #isForwarder!public!testing! !
!MonoClass categoriesFor: #isGeneric!public!testing! !
!MonoClass categoriesFor: #isImported!public!testing! !
!MonoClass categoriesFor: #isInterface!public!testing! !
!MonoClass categoriesFor: #isNested!public! !
!MonoClass categoriesFor: #isNestedAssemblyVisible!public!testing! !
!MonoClass categoriesFor: #isNestedFamilyAndAssemblyVisible!public!testing! !
!MonoClass categoriesFor: #isNestedFamilyOrAssemblyVisible!public!testing! !
!MonoClass categoriesFor: #isNestedFamilyVisible!public!testing! !
!MonoClass categoriesFor: #isNestedPrivate!public!testing! !
!MonoClass categoriesFor: #isNestedPublic!public!testing! !
!MonoClass categoriesFor: #isPublic!public!testing! !
!MonoClass categoriesFor: #isRoot!public!testing! !
!MonoClass categoriesFor: #isRTSpecialName!public!testing! !
!MonoClass categoriesFor: #isSealed!public!testing! !
!MonoClass categoriesFor: #isSequentialLayout!public!testing! !
!MonoClass categoriesFor: #isSerializable!public!testing! !
!MonoClass categoriesFor: #isSpecial!public!testing! !
!MonoClass categoriesFor: #isUnicodeFormat!public!testing! !
!MonoClass categoriesFor: #isValueType!public!testing! !
!MonoClass categoriesFor: #layoutMask!private! !
!MonoClass categoriesFor: #methodFor:!public! !
!MonoClass categoriesFor: #methodNamed:!public! !
!MonoClass categoriesFor: #methods!public! !
!MonoClass categoriesFor: #monoType!public! !
!MonoClass categoriesFor: #name!public! !
!MonoClass categoriesFor: #namespace!public! !
!MonoClass categoriesFor: #newObject!public! !
!MonoClass categoriesFor: #notPublic!public!testing! !
!MonoClass categoriesFor: #outer!public! !
!MonoClass categoriesFor: #properties!public! !
!MonoClass categoriesFor: #rank!accessing!public! !
!MonoClass categoriesFor: #reservedMask!private! !
!MonoClass categoriesFor: #smalltalkClassName!public! !
!MonoClass categoriesFor: #smalltalkName!public! !
!MonoClass categoriesFor: #smalltalkSuperclass!public! !
!MonoClass categoriesFor: #smalltalkSuperclassCount!public! !
!MonoClass categoriesFor: #staticConstructor!public! !
!MonoClass categoriesFor: #stringFormatMask!private! !
!MonoClass categoriesFor: #superclass!public! !
!MonoClass categoriesFor: #superclasses!public! !
!MonoClass categoriesFor: #visibilityMask!private! !
!MonoClass categoriesFor: #vTable!public! !

!MonoClass class methodsFor!

array
	"Answer a new instance of the receiver for the System.Array built-in CLI type."

	^MonoLibrary default monoGetArrayClass!

boolean
	"Answer a new instance of the receiver for the 'bool' (System.Boolean) built-in CLI type."

	^MonoLibrary default monoGetBooleanClass!

byte
	"Answer a new instance of the receiver for the 'byte' (System.Byte) built-in CLI type."

	^MonoLibrary default monoGetByteClass!

double
	"Answer a new instance of the receiver for the 'double' (System.Double) built-in CLI type."

	^MonoLibrary default monoGetDoubleClass!

dword
	"Answer a new instance of the receiver for the 'uint' (System.UInt32) built-in CLI type."

	^MonoLibrary default monoGetUInt32Class!

enum
	"Answer a new instance of the receiver for the 'enum' (System.Enum) built-in CLI type."

	^MonoLibrary default monoGetEnumClass!

exception
	"Answer a new instance of the receiver for the System.Exception built-in CLI type."

	^MonoLibrary default monoGetExceptionClass!

float
	"Answer a new instance of the receiver for the 'float' (System.Single) built-in CLI type."

	^MonoLibrary default monoGetSingleClass!

lpvoid
	"Answer a new instance of the receiver for the System.IntPrt built-in CLI type."

	^MonoLibrary default monoGetIntPtrClass!

object
	"Answer a new instance of the receiver for the 'object' (System.Object) built-in CLI type."

	^MonoLibrary default monoGetObjectClass!

qword
	"Answer a new instance of the receiver for the 'ulong' (System.UInt64) built-in CLI type."

	^MonoLibrary default monoGetUInt64Class!

sbyte
	"Answer a new instance of the receiver for the 'sbyte' (System.SByte) built-in CLI type."

	^MonoLibrary default monoGetSByteClass!

sdword
	"Answer a new instance of the receiver for the 'int' (System.Int32) built-in CLI type."

	^MonoLibrary default monoGetInt32Class!

sqword
	"Answer a new instance of the receiver for the 'long' (System.Int64) built-in CLI type."

	^MonoLibrary default monoGetInt64Class!

string
	"Answer a new instance of the receiver for the 'string' (System.String) built-in CLI type."

	^MonoLibrary default monoGetStringClass!

sword
	"Answer a new instance of the receiver for the 'short' (System.Int16) built-in CLI type."

	^MonoLibrary default monoGetInt16Class!

thread
	"Answer a new instance of the receiver for the System.Threading.Thread built-in CLI type."

	^MonoLibrary default monoGetThreadClass!

uIntPtr
	"Answer a new instance of the receiver for the System.UIntPrt built-in CLI type."

	^MonoLibrary default monoGetUIntPtrClass!

void
	"Answer a new instance of the receiver for the 'void' (System.Void) built-in CLI type."

	^MonoLibrary default monoGetVoidClass!

word
	"Answer a new instance of the receiver for the 'char' (System.Char) built-in CLI type."

	^MonoLibrary default monoGetCharClass! !
!MonoClass class categoriesFor: #array!instance creation!public! !
!MonoClass class categoriesFor: #boolean!instance creation!public! !
!MonoClass class categoriesFor: #byte!instance creation!public! !
!MonoClass class categoriesFor: #double!instance creation!public! !
!MonoClass class categoriesFor: #dword!instance creation!public! !
!MonoClass class categoriesFor: #enum!instance creation!public! !
!MonoClass class categoriesFor: #exception!instance creation!public! !
!MonoClass class categoriesFor: #float!instance creation!public! !
!MonoClass class categoriesFor: #lpvoid!instance creation!public! !
!MonoClass class categoriesFor: #object!instance creation!public! !
!MonoClass class categoriesFor: #qword!instance creation!public! !
!MonoClass class categoriesFor: #sbyte!instance creation!public! !
!MonoClass class categoriesFor: #sdword!instance creation!public! !
!MonoClass class categoriesFor: #sqword!instance creation!public! !
!MonoClass class categoriesFor: #string!instance creation!public! !
!MonoClass class categoriesFor: #sword!instance creation!public! !
!MonoClass class categoriesFor: #thread!instance creation!public! !
!MonoClass class categoriesFor: #uIntPtr!instance creation!public! !
!MonoClass class categoriesFor: #void!instance creation!public! !
!MonoClass class categoriesFor: #word!instance creation!public! !

MonoClassField guid: (GUID fromString: '{D93011BC-9D9F-4BCD-B82C-D5A13626ED13}')!
MonoClassField comment: ''!
!MonoClassField categoriesForClass!External-Data-Unstructured! !
!MonoClassField methodsFor!

attributes
	"Private - Answer the receiver's attributes."

	^MonoLibrary default monoFieldGetFlags: self!

fieldAccessMask
	"Private - Specifies the access level of a given field."

	^self attributes bitAnd: MONO_FIELD_ATTR_FIELD_ACCESS_MASK!

fieldClass
	"Answer the <MonoClass> to which the receiver belongs."

	^MonoLibrary default monoFieldGetParent: self!

hasDefaultValue
	"Answer whether the receiver has a default value."

	^self reservedMask = MONO_FIELD_ATTR_HAS_DEFAULT!

hasMarshallingInformation
	"Answer whether the receiver has marshalling information."

	^self reservedMask = MONO_FIELD_ATTR_HAS_MARSHAL!

hasRVA
	"Answer whether the receiver has a relative virtual address (RVA)."

	^self reservedMask = MONO_FIELD_ATTR_HAS_RVA!

isAssemblyAccessible
	"Answer whether the receiver is accessible throughout the assembly."

	^self fieldAccessMask = MONO_FIELD_ATTR_ASSEMBLY!

isClassVariable
	"Answer whether the receiver represents a class variable."

	^self isStatic!

isCompilerControlled
	"Answer whether the receiver cannot be referenced."

	^self fieldAccessMask = MONO_FIELD_ATTR_COMPILER_CONTROLLED!

isCompilerGenerated
	"Answer whether the receiver is compiler generated and cannot be referenced.."

	^self name first == $<!

isFamilyAccessible
	"Answer whether the receiver is accessible only by type and subtypes."

	^self fieldAccessMask = MONO_FIELD_ATTR_FAMILY!

isFamilyAndAssemblyAccessible
	"Answer whether the receiver is accessible only by subtypes in this assembly."

	^self fieldAccessMask = MONO_FIELD_ATTR_FAM_AND_ASSEM!

isFamilyOrAssemblyAccessible
	"Answer whether the receiver is accessible by subtypes anywhere, as well as throughout
	this assembly."

	^self fieldAccessMask = MONO_FIELD_ATTR_FAM_OR_ASSEM!

isInitializedOnly
	"Answer whether the receiver is initialized only, and can be set only in the body of a
	constructor."

	^(self attributes bitAnd: MONO_FIELD_ATTR_INIT_ONLY) asBoolean!

isInternal
	"Answer whether the receiver is the internal field 'value__'."

	^self name = 'value__'!

isLiteral
	"Answer whether the receiver's value is a compile-time (static or early bound) constant.
	Any attempt to set it throws a FieldAccessException."

	^(self attributes bitAnd: MONO_FIELD_ATTR_LITERAL) asBoolean!

isPInvokeImplemented
	"Private - Reserved for future use."

	^(self attributes bitAnd: MONO_FIELD_ATTR_PINVOKE_IMPL) asBoolean!

isPrivate
	"Answer whether the receiver is accessible only by the parent type."

	^self fieldAccessMask = MONO_FIELD_ATTR_PRIVATE!

isPublic
	"Answer whether the receiver is accessible by any member for whom this scope is visible."

	^self fieldAccessMask = MONO_FIELD_ATTR_PUBLIC!

isRTSpecialName
	"Answer whether the common language runtime (metadata internal APIs) should check the
	name encoding."

	^self reservedMask = MONO_FIELD_ATTR_RT_SPECIAL_NAME!

isSpecial
	"Answer whether the receiver is special, with the name describing how the field is special."

	^(self attributes bitAnd: MONO_FIELD_ATTR_SPECIAL_NAME) asBoolean!

isStatic
	"Answer whether the receiver represents the defined type, or else it is per-instance."

	^(self attributes bitAnd: MONO_FIELD_ATTR_STATIC) asBoolean!

monoType
	"Answer the <MonoType> the receiver represents."

	^MonoLibrary default monoFieldGetType: self!

name
	"Answer the receiver's name."

	^MonoLibrary default monoFieldGetName: self!

notSerialized
	"Answer whether the receiver does not have to be serialized when the type is remoted."

	^(self attributes bitAnd: MONO_FIELD_ATTR_NOT_SERIALIZED) asBoolean!

reservedMask
	"Private - Answer a reserved flag for runtime use only."

	^self attributes bitAnd: MONO_FIELD_ATTR_RESERVED_MASK!

value: anObject for: aMonoObject 
	"Set the receiver's value for aMonoObject to anObject."

	| value |
	value := self monoType canBeMarshalled 
				ifTrue: [self monoType smalltalkClass fromObject: anObject]
				ifFalse: [anObject].
	self isClassVariable 
		ifTrue: 
			[MonoLibrary default 
				monoFieldStaticSetValue: aMonoObject monoClass vTable
				field: self
				value: value yourAddress]
		ifFalse: 
			[MonoLibrary default 
				monoFieldSetValue: aMonoObject
				field: self
				value: value yourAddress]!

valueFor: aMonoObject 
	"Answer the receiver's value for aMonoObject."

	| value |
	value := MonoLibrary default 
				monoFieldGetValueObject: MonoDomain current
				field: self
				obj: aMonoObject yourAddress.
	^value initializePointer asObject! !
!MonoClassField categoriesFor: #attributes!private! !
!MonoClassField categoriesFor: #fieldAccessMask!private! !
!MonoClassField categoriesFor: #fieldClass!public! !
!MonoClassField categoriesFor: #hasDefaultValue!public!testing! !
!MonoClassField categoriesFor: #hasMarshallingInformation!public!testing! !
!MonoClassField categoriesFor: #hasRVA!public!testing! !
!MonoClassField categoriesFor: #isAssemblyAccessible!public!testing! !
!MonoClassField categoriesFor: #isClassVariable!public!testing! !
!MonoClassField categoriesFor: #isCompilerControlled!public!testing! !
!MonoClassField categoriesFor: #isCompilerGenerated!public!testing! !
!MonoClassField categoriesFor: #isFamilyAccessible!public!testing! !
!MonoClassField categoriesFor: #isFamilyAndAssemblyAccessible!public!testing! !
!MonoClassField categoriesFor: #isFamilyOrAssemblyAccessible!public!testing! !
!MonoClassField categoriesFor: #isInitializedOnly!public!testing! !
!MonoClassField categoriesFor: #isInternal!public!testing! !
!MonoClassField categoriesFor: #isLiteral!public!testing! !
!MonoClassField categoriesFor: #isPInvokeImplemented!private!testing! !
!MonoClassField categoriesFor: #isPrivate!public!testing! !
!MonoClassField categoriesFor: #isPublic!public!testing! !
!MonoClassField categoriesFor: #isRTSpecialName!public!testing! !
!MonoClassField categoriesFor: #isSpecial!public!testing! !
!MonoClassField categoriesFor: #isStatic!public!testing! !
!MonoClassField categoriesFor: #monoType!public! !
!MonoClassField categoriesFor: #name!public! !
!MonoClassField categoriesFor: #notSerialized!public!testing! !
!MonoClassField categoriesFor: #reservedMask!private! !
!MonoClassField categoriesFor: #value:for:!accessing!public! !
!MonoClassField categoriesFor: #valueFor:!accessing!public! !

MonoDomain guid: (GUID fromString: '{D4C7521D-7B67-4E45-ABBD-C78858E11ECD}')!
MonoDomain comment: ''!
!MonoDomain categoriesForClass!External-Data-Unstructured! !
!MonoDomain methodsFor!

assemblyFromFile: aFileNameString 
	"Load a <MonoAssembly> from aFileNameString into the receiver and answer it."

	| assembly |
	assembly := MonoLibrary default monoDomainAssemblyOpen: self name: aFileNameString.
	^assembly isNull 
		ifTrue: [self error: 'Can''t load assembly from: ' , aFileNameString]
		ifFalse: [assembly]! !
!MonoDomain categoriesFor: #assemblyFromFile:!public! !

!MonoDomain class methodsFor!

current
	"Answer the current <MonoDomain>."

	"^MonoLibrary default monoDomainGet"
	^MonoRuntime current domain! !
!MonoDomain class categoriesFor: #current!accessing!public! !

MonoImage guid: (GUID fromString: '{AB895AEE-B39E-459C-AC33-4BBA0D9557C2}')!
MonoImage comment: ''!
!MonoImage categoriesForClass!External-Data-Unstructured! !
!MonoImage methodsFor!

allClasses
	"Answer a collection of all of the <MonoClass>es in the receiver, in breadth-first order."

	^self classes asSortedCollection: [:class1 :class2 | class1 smalltalkSuperclassCount <= class2 smalltalkSuperclassCount]!

assemblyOSTableNumRows
	"Answer the number of rows in the AssemblyOS table."

	^self numRowsIn: MONO_TABLE_ASSEMBLYOS!

assemblyProcessorTableNumRows
	"Answer the number of rows in the AssemblyProcessor table."

	^self numRowsIn: MONO_TABLE_ASSEMBLYPROCESSOR!

assemblyRefOSTableNumRows
	"Answer the number of rows in the AssemblyRefOS table."

	^self numRowsIn: MONO_TABLE_ASSEMBLYREFOS!

assemblyRefProcessorTableNumRows
	"Answer the number of rows in the AssemblyRefProcessor table."

	^self numRowsIn: MONO_TABLE_ASSEMBLYREFPROCESSOR!

assemblyRefTableNumRows
	"Answer the number of rows in the AssemblyRef table."

	^self numRowsIn: MONO_TABLE_ASSEMBLYREF!

assemblyTableNumRows
	"Answer the number of rows in the Assembly table."

	^self numRowsIn: MONO_TABLE_ASSEMBLY!

class: aClassName namespace: aNamespaceString 
	"Answer the <MonoClass> named aClassName within the aNamespaceString namespace."

	| class |
	class := MonoLibrary default 
				monoClassFromName: self
				namespace: aNamespaceString
				name: aClassName.
	^class isNull 
		ifTrue: [self errorNotFound: aNamespaceString , '.' , aClassName]
		ifFalse: [class initialize]!

classAt: anInteger 
	"Answer the <MonoClass> at anInteger index."

	^MonoLibrary default monoClassGet: self typeToken: (anInteger bitOr: MONO_TOKEN_TYPE_DEF)!

classCount
	"Answer the number of <MonoClass>es in the receiver."

	^self typeDefTableNumRows!

classes
	"Answer a collection of all of the <MonoClass>es in the receiver."

	| classes |
	classes := OrderedCollection new.
	1 to: self classCount do: [:index | classes add: (self classAt: index)].
	^classes notEmpty 
		ifTrue: 
			["we don't want the <Module> type"
			classes allButFirst]
		ifFalse: [classes]!

classLayoutTableNumRows
	"Answer the number of rows in the ClassLayout table."

	^self numRowsIn: MONO_TABLE_CLASSLAYOUT!

constantTableNumRows
	"Answer the number of rows in the Constant table."

	^self numRowsIn: MONO_TABLE_CONSTANT!

customAttributeTableNumRows
	"Answer the number of rows in the CustomAttribute table."

	^self numRowsIn: MONO_TABLE_CUSTOMATTRIBUTE!

declSecurityTableNumRows
	"Answer the number of rows in the DeclSecurity table."

	^self numRowsIn: MONO_TABLE_DECLSECURITY!

encLogTableNumRows
	"Answer the number of rows in the ENCLog table."

	^self numRowsIn: MONO_TABLE_UNUSED6!

encMapTableNumRows
	"Answer the number of rows in the ENCMap table."

	^self numRowsIn: MONO_TABLE_UNUSED7!

eventMapTableNumRows
	"Answer the number of rows in the EventMap table."

	^self numRowsIn: MONO_TABLE_EVENTMAP!

eventPointerTableNumRows
	"Answer the number of rows in the EventPointer table."

	^self numRowsIn: MONO_TABLE_EVENT_POINTER!

eventTableNumRows
	"Answer the number of rows in the Event table."

	^self numRowsIn: MONO_TABLE_EVENT!

exportedTypeTableNumRows
	"Answer the number of rows in the ExportedType table."

	^self numRowsIn: MONO_TABLE_EXPORTEDTYPE!

fieldLayoutTableNumRows
	"Answer the number of rows in the FieldLayout table."

	^self numRowsIn: MONO_TABLE_FIELDLAYOUT!

fieldMarshalTableNumRows
	"Answer the number of rows in the FieldMarshal table."

	^self numRowsIn: MONO_TABLE_FIELDMARSHAL!

fieldPointerTableNumRows
	"Answer the number of rows in the FieldPointer table."

	^self numRowsIn: MONO_TABLE_FIELD_POINTER!

fieldRVATableNumRows
	"Answer the number of rows in the FieldRVA table."

	^self numRowsIn: MONO_TABLE_FIELDRVA!

fieldTableNumRows
	"Answer the number of rows in the Field table."

	^self numRowsIn: MONO_TABLE_FIELD!

fileName
	"Answer the name of the file associated with the receiver."

	^MonoLibrary default monoImageGetFilename: self!

fileTableNumRows
	"Answer the number of rows in the File table."

	^self numRowsIn: MONO_TABLE_FILE!

generateWrappers
	"Generate wrappers for the receiver's classes."

	| classCount classMethodCount instanceMethodCount |
	classCount := classMethodCount := instanceMethodCount := 0.
	self allClasses do: 
			[:monoClass | 
			(MonoClassBuilder on: monoClass) build 
				ifNotNil: 
					[:class | 
					classCount := classCount + 1.
					classMethodCount := classMethodCount + class metaClass methodDictionary size.
					instanceMethodCount := instanceMethodCount + class methodDictionary size]]!

genericParamConstraintTableNumRows
	"Answer the number of rows in the GenericParamConstraint table."

	^self numRowsIn: MONO_TABLE_GENERICPARAMCONSTRAINT!

genericParamTableNumRows
	"Answer the number of rows in the GenericParam table."

	^self numRowsIn: MONO_TABLE_GENERICPARAM!

implMapTableNumRows
	"Answer the number of rows in the ImplMap table."

	^self numRowsIn: MONO_TABLE_IMPLMAP!

interfaceImplTableNumRows
	"Answer the number of rows in the InterfaceImpl table."

	^self numRowsIn: MONO_TABLE_INTERFACEIMPL!

manifestResourceTableNumRows
	"Answer the number of rows in the ManifestResource table."

	^self numRowsIn: MONO_TABLE_MANIFESTRESOURCE!

memberRefTableNumRows
	"Answer the number of rows in the MemberRef table."

	^self numRowsIn: MONO_TABLE_MEMBERREF!

methodImplTableNumRows
	"Answer the number of rows in the MethodImpl table."

	^self numRowsIn: MONO_TABLE_METHODIMPL!

methodPointerTableNumRows
	"Answer the number of rows in the MethodPointer table."

	^self numRowsIn: MONO_TABLE_METHOD_POINTER!

methodSemanticsTableNumRows
	"Answer the number of rows in the MethodSemantics table."

	^self numRowsIn: MONO_TABLE_METHODSEMANTICS!

methodSpecTableNumRows
	"Answer the number of rows in the MethodSpec table."

	^self numRowsIn: MONO_TABLE_METHODSPEC!

methodTableNumRows
	"Answer the number of rows in the Method table."

	^self numRowsIn: MONO_TABLE_METHOD!

moduleRefTableNumRows
	"Answer the number of rows in the ModuleRef table."

	^self numRowsIn: MONO_TABLE_MODULEREF!

moduleTableNumRows
	"Answer the number of rows in the Module table."

	^self numRowsIn: MONO_TABLE_MODULE!

name
	"Answer the receiver's name."

	^MonoLibrary default monoImageGetName: self!

nestedClassTableNumRows
	"Answer the number of rows in the NestedClass table."

	^self numRowsIn: MONO_TABLE_NESTEDCLASS!

numRowsIn: anIntegerTableId 
	"Private - Answer the number of rows in anIntegerTableId."

	^MonoLibrary default monoImageGetTableRows: self tableId: anIntegerTableId!

paramPointerTableNumRows
	"Answer the number of rows in the ParamPointer table."

	^self numRowsIn: MONO_TABLE_PARAM_POINTER!

paramTableNumRows
	"Answer the number of rows in the Param table."

	^self numRowsIn: MONO_TABLE_PARAM!

propertyMapTableNumRows
	"Answer the number of rows in the PropertyMap table."

	^self numRowsIn: MONO_TABLE_PROPERTYMAP!

propertyPointerTableNumRows
	"Answer the number of rows in the PropertyPointer table."

	^self numRowsIn: MONO_TABLE_PROPERTY_POINTER!

propertyTableNumRows
	"Answer the number of rows in the Property table."

	^self numRowsIn: MONO_TABLE_PROPERTY!

smalltalkName
	"Answer the receiver's Smalltalk name."

	^self name copyReplacing: $. withObject: $_!

standAloneSigTableNumRows
	"Answer the number of rows in the StandAloneSig table."

	^self numRowsIn: MONO_TABLE_STANDALONESIG!

typeDefTableNumRows
	"Answer the number of rows in the TypeDef table."

	^self numRowsIn: MONO_TABLE_TYPEDEF!

typeRefTableNumRows
	"Answer the number of rows in the TypeRef table."

	^self numRowsIn: MONO_TABLE_TYPEREF!

typeSpecTableNumRows
	"Answer the number of rows in the TypeSpec table."

	^self numRowsIn: MONO_TABLE_TYPESPEC! !
!MonoImage categoriesFor: #allClasses!public! !
!MonoImage categoriesFor: #assemblyOSTableNumRows!public! !
!MonoImage categoriesFor: #assemblyProcessorTableNumRows!public! !
!MonoImage categoriesFor: #assemblyRefOSTableNumRows!public! !
!MonoImage categoriesFor: #assemblyRefProcessorTableNumRows!public! !
!MonoImage categoriesFor: #assemblyRefTableNumRows!public! !
!MonoImage categoriesFor: #assemblyTableNumRows!public! !
!MonoImage categoriesFor: #class:namespace:!public! !
!MonoImage categoriesFor: #classAt:!public! !
!MonoImage categoriesFor: #classCount!public! !
!MonoImage categoriesFor: #classes!public! !
!MonoImage categoriesFor: #classLayoutTableNumRows!public! !
!MonoImage categoriesFor: #constantTableNumRows!public! !
!MonoImage categoriesFor: #customAttributeTableNumRows!public! !
!MonoImage categoriesFor: #declSecurityTableNumRows!public! !
!MonoImage categoriesFor: #encLogTableNumRows!public! !
!MonoImage categoriesFor: #encMapTableNumRows!public! !
!MonoImage categoriesFor: #eventMapTableNumRows!public! !
!MonoImage categoriesFor: #eventPointerTableNumRows!public! !
!MonoImage categoriesFor: #eventTableNumRows!public! !
!MonoImage categoriesFor: #exportedTypeTableNumRows!public! !
!MonoImage categoriesFor: #fieldLayoutTableNumRows!public! !
!MonoImage categoriesFor: #fieldMarshalTableNumRows!public! !
!MonoImage categoriesFor: #fieldPointerTableNumRows!public! !
!MonoImage categoriesFor: #fieldRVATableNumRows!public! !
!MonoImage categoriesFor: #fieldTableNumRows!public! !
!MonoImage categoriesFor: #fileName!public! !
!MonoImage categoriesFor: #fileTableNumRows!public! !
!MonoImage categoriesFor: #generateWrappers!public! !
!MonoImage categoriesFor: #genericParamConstraintTableNumRows!public! !
!MonoImage categoriesFor: #genericParamTableNumRows!public! !
!MonoImage categoriesFor: #implMapTableNumRows!public! !
!MonoImage categoriesFor: #interfaceImplTableNumRows!public! !
!MonoImage categoriesFor: #manifestResourceTableNumRows!public! !
!MonoImage categoriesFor: #memberRefTableNumRows!public! !
!MonoImage categoriesFor: #methodImplTableNumRows!public! !
!MonoImage categoriesFor: #methodPointerTableNumRows!public! !
!MonoImage categoriesFor: #methodSemanticsTableNumRows!public! !
!MonoImage categoriesFor: #methodSpecTableNumRows!public! !
!MonoImage categoriesFor: #methodTableNumRows!public! !
!MonoImage categoriesFor: #moduleRefTableNumRows!public! !
!MonoImage categoriesFor: #moduleTableNumRows!public! !
!MonoImage categoriesFor: #name!public! !
!MonoImage categoriesFor: #nestedClassTableNumRows!public! !
!MonoImage categoriesFor: #numRowsIn:!private! !
!MonoImage categoriesFor: #paramPointerTableNumRows!public! !
!MonoImage categoriesFor: #paramTableNumRows!public! !
!MonoImage categoriesFor: #propertyMapTableNumRows!public! !
!MonoImage categoriesFor: #propertyPointerTableNumRows!public! !
!MonoImage categoriesFor: #propertyTableNumRows!public! !
!MonoImage categoriesFor: #smalltalkName!public! !
!MonoImage categoriesFor: #standAloneSigTableNumRows!public! !
!MonoImage categoriesFor: #typeDefTableNumRows!public! !
!MonoImage categoriesFor: #typeRefTableNumRows!public! !
!MonoImage categoriesFor: #typeSpecTableNumRows!public! !

!MonoImage class methodsFor!

errorTextFor: anInteger 
	"Answer a description of the specified anInteger error code."

	^MonoLibrary default monoImageStrError: anInteger!

mscorlib
	"Answer the mscorlib assembly image."

	^MonoLibrary default monoGetCorlib! !
!MonoImage class categoriesFor: #errorTextFor:!public! !
!MonoImage class categoriesFor: #mscorlib!public! !

MonoMethod guid: (GUID fromString: '{D3CDF4AF-3108-4EBB-822B-42B25E1DE44C}')!
MonoMethod comment: ''!
!MonoMethod categoriesForClass!External-Data-Unstructured! !
!MonoMethod methodsFor!

accessorName
	"Answer the receiver's accessor name."

	| shortName |
	self isAccessor ifFalse: [^String empty].
	shortName := self shortName.
	^self isGetter 
		ifTrue: [shortName allButFirst: self getterPrefix size]
		ifFalse: [shortName allButFirst: self setterPrefix size]!

argumentCount
	"Answer the number of parameters expected by the receiver."

	^self signature argumentCount!

argumentNames
	"Answer an array containing the names of the receiver's parameters."

	| argumentNames |
	argumentNames := PointerArray length: self argumentCount elementClass: String.
	MonoLibrary default monoMethodGetParamNames: self names: argumentNames.
	^argumentNames asArray!

argumentTypes
	"Answer a collection of the argument types expected by the receiver in left to right order."

	^self signature argumentTypes!

attributes
	"Private - Answer the receiver's attributes."

	^MonoLibrary default monoMethodGetFlags: self iflags: nil!

constructorName
	"Private - Answer the constructor method name."

	^'.ctor'!

fullName
	"Answer the receiver's full name without signature."

	^self fullName: false!

fullName: aBoolean 
	"Private - Answer the receiver's full name with signature according to aBoolean."

	^MonoLibrary default monoMethodFullName: self signature: aBoolean!

fullNameWithSignature
	"Answer the receiver's full name with signature."

	^self fullName: true!

getsNewSlot
	"Answer whether the receiver always gets a new slot in the vtable."

	^self vtableLayoutMask = MONO_METHOD_ATTR_NEW_SLOT!

getterPrefix
	"Private - Answer the receiver's getter method prefix."

	^'get_'!

handleException: aMonoObject 
	"Private - Handle the exception wrapped in aMonoObject."

	#todo.
	^self error: 'An exception (' , aMonoObject monoClass fullyQualifiedName , ') was signalled in ' , self fullName!

hasSecurity
	"Answer whether the receiver has security associated with it.
	Reserved flag for runtime use only."

	^self reservedMask = MONO_METHOD_ATTR_HAS_SECURITY!

hidesBySignature
	"Answer whether the receiver hides by name and signature; otherwise, by name only."

	^(self attributes bitAnd: MONO_METHOD_ATTR_HIDE_BY_SIG) asBoolean!

interfaceArgumentCount
	"Answer the argument count of the interface the receiver implements."

	| interfaceFullName startIndex stopIndex interfaceArguments writeStream readStream |
	#todo.	"refactor this method"
	self isGenericInterfaceImplementation ifFalse: [^0].
	interfaceFullName := self interfaceFullName.
	startIndex := interfaceFullName indexOf: $<.
	stopIndex := interfaceFullName lastIndexOf: $>.
	interfaceArguments := interfaceFullName copyFrom: startIndex + 1 to: stopIndex - 1.
	writeStream := String writeStream.
	readStream := interfaceArguments readStream.
	[readStream atEnd] whileFalse: 
			[readStream peek == $< ifTrue: [readStream skipTo: $>].
			readStream atEnd ifFalse: [writeStream nextPut: readStream next]].
	^(writeStream contents subStrings: $,) size!

interfaceFullName
	"Answer the interface full name the receiver implements."

	| methodName interfaceNameLength |
	methodName := self name.
	interfaceNameLength := (methodName lastIndexOf: $. ifAbsent: [^String empty]) - 1.
	^methodName first: interfaceNameLength!

interfaceMethodName
	"Answer the interface method name of the receiver."

	self isInterfaceImplementation ifFalse: [^String empty].
	^self interfaceName , '_' , self interfaceSuffixName!

interfaceName
	"Answer the interface name the receiver implements."

	| interfaceArgumentCount |
	interfaceArgumentCount := self interfaceArgumentCount.
	^self interfaceArgumentCount isZero 
		ifTrue: [self interfacePrefixName]
		ifFalse: [self interfacePrefixName , '_' , interfaceArgumentCount printString]!

interfacePrefixName
	"Private - Answer the interface prefix name the receiver implements."

	^((self interfaceFullName subStrings: $<) first subStrings: $.) last!

interfaceSuffixName
	"Private - Answer the interface suffix name the receiver implements."

	| methodName |
	methodName := self name.
	^methodName last: methodName size - self interfaceFullName size - 1!

isAbstract
	"Answer whether the class does not provide an implementation of the receiver."

	^self vtableLayoutMask = MONO_METHOD_ATTR_ABSTRACT!

isAccessor
	"Answer whether the receiver is an accessor method."

	^self isGetter or: [self isSetter]!

isAssemblyAccessible
	"Answer whether the receiver is accessible to any class of this assembly."

	^self memberAccessMask = MONO_METHOD_ATTR_ASSEM!

isBinary
	"Answer whether the receiver implements a binary message send."

	^self isOperator and: [self argumentCount == 2]!

isClassMethod
	"Answer true if the receiver is a class method."

	^self isStatic!

isCompilerControlled
	"Answer whether the receiver cannot be referenced."

	^self memberAccessMask = MONO_METHOD_ATTR_COMPILER_CONTROLLED!

isCompilerGenerated
	"Answer whether the receiver is compiler generated and cannot be referenced.."

	^self name first == $<!

isConstructor
	"Answer whether the receiver is a constructor method."

	| methodName |
	methodName := self name.
	^methodName = self constructorName or: [methodName = self staticConstructorName]!

isDefaultConstructor
	"Answer whether the receiver is the default constructor method."

	^self isConstructor and: [self argumentCount isZero]!

isFamilyAccessible
	"Answer whether the receiver is accessible only to members of this class and its derived
	classes."

	^self memberAccessMask = MONO_METHOD_ATTR_FAMILY!

isFamilyAndAssemblyAccessible
	"Answer whether the receiver is accessible to members of this type and its derived types
	that are in this assembly only."

	^self memberAccessMask = MONO_METHOD_ATTR_FAM_AND_ASSEM!

isFamilyOrAssemblyAccessible
	"Answer whether the receiver is accessible to derived classes anywhere, as well as to any
	class in the assembly."

	^self memberAccessMask = MONO_METHOD_ATTR_FAM_OR_ASSEM!

isFinal
	"Answer whether the receiver cannot be overridden."

	^(self attributes bitAnd: MONO_METHOD_ATTR_FINAL) asBoolean!

isGenericInterfaceImplementation
	"Answer whether the receiver implements a Generic interface method."

	^self name includes: $<!

isGetter
	"Answer whether the receiver is a getter method."

	^self isSpecial and: [self shortName beginsWith: self getterPrefix]!

isInterfaceImplementation
	"Answer whether the receiver implements an interface method."

	^self isConstructor not and: [self isStaticConstructor not and: [self name includes: $.]]!

isKeyword
	"Answer whether the receiver implements a keyword message send."

	^self isUnary not and: [self isBinary not]!

isOperator
	"Answer whether the receiver is an operator method."

	^self isSpecial and: [self name beginsWith: 'op_']!

isPInvokeImplemented
	"Answer whether the receiver's implementation is forwarded through PInvoke (Platform
	Invocation Services)."

	^(self attributes bitAnd: MONO_METHOD_ATTR_PINVOKE_IMPL) asBoolean!

isPrivate
	"Answer whether the receiver is accessible only to the current class."

	^self memberAccessMask = MONO_METHOD_ATTR_PRIVATE!

isPublic
	"Answer whether the receiver is accessible to any object for which this object is in scope."

	^self memberAccessMask = MONO_METHOD_ATTR_PUBLIC!

isRTSpecialName
	"Answer whether the common language runtime checks the receiver's name encoding."

	^self reservedMask = MONO_METHOD_ATTR_RT_SPECIAL_NAME!

isSetter
	"Answer whether the receiver is a setter method."

	^self isSpecial and: [self shortName beginsWith: self setterPrefix]!

isSpecial
	"Answer whether the receiver is special. The name describes how this method is special."

	^(self attributes bitAnd: MONO_METHOD_ATTR_SPECIAL_NAME) asBoolean!

isStatic
	"Answer whether the receiver is defined on the type; otherwise, it is defined per instance."

	^(self attributes bitAnd: MONO_METHOD_ATTR_STATIC) asBoolean!

isStaticConstructor
	"Answer whether the receiver is a static constructor method."

	^self name = self staticConstructorName!

isStrict
	"Answer whether the receiver can only be overridden when it is also accessible."

	^self vtableLayoutMask = MONO_METHOD_ATTR_STRICT!

isUnary
	"Answer whether the receiver implements an unary message send."

	| argumentCount |
	argumentCount := self argumentCount.
	^argumentCount isZero or: [self isOperator and: [argumentCount == 1]]!

isUnaryOperator
	"Answer whether the receiver represents an unary operator."

	^self isUnary and: [self isOperator]!

isUnmanagedExported
	"Answer whether the receiver is exported by thunk to unmanaged code."

	^(self attributes bitAnd: MONO_METHOD_ATTR_UNMANAGED_EXPORT) asBoolean!

isVirtual
	"Answer whether the receiver is virtual."

	^(self attributes bitAnd: MONO_METHOD_ATTR_VIRTUAL) asBoolean!

marshalArguments: aSequenceableCollection 
	"Private - Answer aSequenceableCollection in a form suitable for passing to the
	MonoLibrary>>monoRuntimeInvoke:obj:params:exc: external function."

	| arguments argumentCount argumentTypes |
	#todo.	"refactor this method"
	aSequenceableCollection isEmpty ifTrue: [^nil].
	argumentCount := aSequenceableCollection size.
	argumentTypes := self argumentTypes.
	arguments := PointerArray length: argumentCount elementClass: VOID.
	1 to: argumentCount
		do: 
			[:index | 
			| argumentType class element argument |
			argumentType := argumentTypes at: index.
			class := argumentType smalltalkClass.
			element := aSequenceableCollection at: index.
			argument := (element isMonoObject or: [class == element class]) 
						ifTrue: [element]
						ifFalse: [class fromObject: element].
			argumentType isValueType ifTrue: [argument := argument yourAddress].
			arguments at: index put: argument].
	^arguments!

memberAccessMask
	"Private - Retrieves accessibility information."

	^self attributes bitAnd: MONO_METHOD_ATTR_ACCESS_MASK!

methodClass
	"Answer the <MonoClass> to which the receiver belongs."

	^MonoLibrary default monoMethodGetClass: self!

monoProperty
	"Answer the <MonoProperty> the receiver wraps."

	^self isAccessor 
		ifTrue: 
			[self methodClass properties detect: 
					[:property | 
					| propertyMethod |
					propertyMethod := self isGetter ifTrue: [property getterMethod] ifFalse: [property setterMethod].
					propertyMethod isNull 
						ifFalse: [property name = self propertyName and: [propertyMethod argumentTypes = self argumentTypes]]
						ifTrue: [false]]]
		ifFalse: [MonoProperty newPointer]!

name
	"Answer the receiver's name."

	^MonoLibrary default monoMethodGetName: self!

propertyName
	"Answer the name of the <MonoProperty> the receiver wraps."

	| accessorName |
	self isAccessor ifFalse: [^String empty].
	accessorName := self accessorName.
	^self isInterfaceImplementation 
		ifTrue: [self interfaceFullName , '.' , accessorName]
		ifFalse: [accessorName]!

requiresSecurityObject
	"Answer whether the receiver calls another method containing security code.
	Reserved flag for runtime use only."

	^self reservedMask = MONO_METHOD_ATTR_REQUIRE_SEC_OBJECT!

reservedMask
	"Private - Answer a reserved flag for runtime use only."

	^self attributes bitAnd: MONO_METHOD_ATTR_RESERVED_MASK!

returnType
	"Answer the receiver's return type."

	^self signature returnType!

setterPrefix
	"Private - Answer the receiver's setter method prefix."

	^'set_'!

shortName
	"Answer the receiver's short name (its name minus its interface name)."

	| name |
	name := self name.
	^self isInterfaceImplementation 
		ifTrue: [name allButFirst: self interfaceFullName size + 1]
		ifFalse: [name]!

signature
	"Answer the receiver's signature."

	^MonoLibrary default monoMethodSignature: self!

smalltalkName
	"Answer the receiver's Smalltalk name."

	^self isConstructor 
		ifTrue: [self methodClass smalltalkName]
		ifFalse: [self isInterfaceImplementation ifTrue: [self interfaceMethodName] ifFalse: [self name]]!

staticConstructorName
	"Private - Answer the static constructor method name."

	^'.cctor'!

value
	"Answer the result of evaluating the receiver.
	
	Implementation Note: Must be used when the receiver is static!!"

	^self valueWithArguments: #()!

value: aMonoObject 
	"Evaluate the receiver with the argument, aMonoObject, as its receiver, answering the result."

	^self value: aMonoObject withArguments: #()!

value: aMonoObject withArguments: aSequenceableCollection 
	"Evaluate the receiver with the argument, aMonoObject, as its receiver, and with the argument,
	aSequenceableCollection, as the arguments to the message."

	| object arguments exception answer |
	#todo.	"refactor this method"
	object := aMonoObject notNil ifTrue: [aMonoObject yourAddress].
	arguments := self marshalArguments: aSequenceableCollection.
	exception := MonoObject newPointer.
	answer := MonoLibrary default 
				monoRuntimeInvoke: self
				obj: object
				params: arguments
				exc: exception.
	exception notNull ifTrue: [^self handleException: exception initializePointer].
	^answer notNull ifTrue: [answer initializePointer asObject]!

valueWithArguments: aSequenceableCollection 
	"Answer the result of evaluating the receiver with arguments from aSequenceableCollection.
	
	Implementation Note: Must be used when the receiver is static!!"

	^self value: nil withArguments: aSequenceableCollection!

vtableLayoutMask
	"Private - Retrieves vtable attributes."

	^self attributes bitAnd: MONO_METHOD_ATTR_VTABLE_LAYOUT_MASK!

willReuseSlot
	"Answer whether the receiver will reuse an existing slot in the vtable.
	This is the default behavior."

	^self vtableLayoutMask = MONO_METHOD_ATTR_REUSE_SLOT! !
!MonoMethod categoriesFor: #accessorName!public! !
!MonoMethod categoriesFor: #argumentCount!public! !
!MonoMethod categoriesFor: #argumentNames!public! !
!MonoMethod categoriesFor: #argumentTypes!public! !
!MonoMethod categoriesFor: #attributes!private! !
!MonoMethod categoriesFor: #constructorName!constants!private! !
!MonoMethod categoriesFor: #fullName!public! !
!MonoMethod categoriesFor: #fullName:!private! !
!MonoMethod categoriesFor: #fullNameWithSignature!public! !
!MonoMethod categoriesFor: #getsNewSlot!public! !
!MonoMethod categoriesFor: #getterPrefix!constants!private! !
!MonoMethod categoriesFor: #handleException:!private! !
!MonoMethod categoriesFor: #hasSecurity!public!testing! !
!MonoMethod categoriesFor: #hidesBySignature!public!testing! !
!MonoMethod categoriesFor: #interfaceArgumentCount!public! !
!MonoMethod categoriesFor: #interfaceFullName!public! !
!MonoMethod categoriesFor: #interfaceMethodName!public! !
!MonoMethod categoriesFor: #interfaceName!public! !
!MonoMethod categoriesFor: #interfacePrefixName!private! !
!MonoMethod categoriesFor: #interfaceSuffixName!private! !
!MonoMethod categoriesFor: #isAbstract!public!testing! !
!MonoMethod categoriesFor: #isAccessor!public!testing! !
!MonoMethod categoriesFor: #isAssemblyAccessible!public!testing! !
!MonoMethod categoriesFor: #isBinary!public!testing! !
!MonoMethod categoriesFor: #isClassMethod!public!testing! !
!MonoMethod categoriesFor: #isCompilerControlled!public!testing! !
!MonoMethod categoriesFor: #isCompilerGenerated!public!testing! !
!MonoMethod categoriesFor: #isConstructor!public!testing! !
!MonoMethod categoriesFor: #isDefaultConstructor!public!testing! !
!MonoMethod categoriesFor: #isFamilyAccessible!public!testing! !
!MonoMethod categoriesFor: #isFamilyAndAssemblyAccessible!public!testing! !
!MonoMethod categoriesFor: #isFamilyOrAssemblyAccessible!public!testing! !
!MonoMethod categoriesFor: #isFinal!public!testing! !
!MonoMethod categoriesFor: #isGenericInterfaceImplementation!public!testing! !
!MonoMethod categoriesFor: #isGetter!public!testing! !
!MonoMethod categoriesFor: #isInterfaceImplementation!public!testing! !
!MonoMethod categoriesFor: #isKeyword!public!testing! !
!MonoMethod categoriesFor: #isOperator!public!testing! !
!MonoMethod categoriesFor: #isPInvokeImplemented!public!testing! !
!MonoMethod categoriesFor: #isPrivate!public!testing! !
!MonoMethod categoriesFor: #isPublic!public!testing! !
!MonoMethod categoriesFor: #isRTSpecialName!public!testing! !
!MonoMethod categoriesFor: #isSetter!public!testing! !
!MonoMethod categoriesFor: #isSpecial!public!testing! !
!MonoMethod categoriesFor: #isStatic!public!testing! !
!MonoMethod categoriesFor: #isStaticConstructor!public!testing! !
!MonoMethod categoriesFor: #isStrict!public!testing! !
!MonoMethod categoriesFor: #isUnary!public!testing! !
!MonoMethod categoriesFor: #isUnaryOperator!public!testing! !
!MonoMethod categoriesFor: #isUnmanagedExported!public!testing! !
!MonoMethod categoriesFor: #isVirtual!public!testing! !
!MonoMethod categoriesFor: #marshalArguments:!private! !
!MonoMethod categoriesFor: #memberAccessMask!private! !
!MonoMethod categoriesFor: #methodClass!accessing!public! !
!MonoMethod categoriesFor: #monoProperty!public! !
!MonoMethod categoriesFor: #name!public! !
!MonoMethod categoriesFor: #propertyName!public! !
!MonoMethod categoriesFor: #requiresSecurityObject!public!testing! !
!MonoMethod categoriesFor: #reservedMask!private! !
!MonoMethod categoriesFor: #returnType!public! !
!MonoMethod categoriesFor: #setterPrefix!constants!private! !
!MonoMethod categoriesFor: #shortName!public! !
!MonoMethod categoriesFor: #signature!public! !
!MonoMethod categoriesFor: #smalltalkName!public! !
!MonoMethod categoriesFor: #staticConstructorName!constants!private! !
!MonoMethod categoriesFor: #value!evaluating!public! !
!MonoMethod categoriesFor: #value:!evaluating!public! !
!MonoMethod categoriesFor: #value:withArguments:!evaluating!public! !
!MonoMethod categoriesFor: #valueWithArguments:!evaluating!public! !
!MonoMethod categoriesFor: #vtableLayoutMask!private! !
!MonoMethod categoriesFor: #willReuseSlot!public!testing! !

MonoMethodDesc guid: (GUID fromString: '{6916CFF5-54BF-493E-A63B-E46F50C16B9C}')!
MonoMethodDesc comment: ''!
!MonoMethodDesc categoriesForClass!External-Data-Unstructured! !
!MonoMethodDesc methodsFor!

basicFree
	"Private - Free external resources held by the receiver."

	MonoLibrary default monoMethodDescFree: self!

methodIn: aMonoClass 
	"Answer the <MonoMethod> described by the receiver in aMonoClass."

	^MonoLibrary default monoMethodDescSearchInClass: self klass: aMonoClass!

needsFree
	"Private - Answer whether the receiver requires freeing of any external resources."

	^self notNull! !
!MonoMethodDesc categoriesFor: #basicFree!private!realizing/unrealizing! !
!MonoMethodDesc categoriesFor: #methodIn:!public! !
!MonoMethodDesc categoriesFor: #needsFree!private!realizing/unrealizing! !

!MonoMethodDesc class methodsFor!

name: aString 
	"Answer a new instance of the receiver with name aString which must include namespace
	information."

	^self name: aString includesNamespace: true!

name: aString includesNamespace: aBoolean 
	"Answer a new instance of the receiver with name aString that includes namespace
	information according to aBoolean."

	^(MonoLibrary default monoMethodDescNew: aString includeNamespace: aBoolean)
		beFinalizable;
		yourself!

new
	"Should not implement. Use #name or #name:includesNamespace:"

	^self shouldNotImplement! !
!MonoMethodDesc class categoriesFor: #name:!instance creation!public! !
!MonoMethodDesc class categoriesFor: #name:includesNamespace:!instance creation!public! !
!MonoMethodDesc class categoriesFor: #new!instance creation!public! !

MonoMethodSignature guid: (GUID fromString: '{9EE98F81-7376-4851-B177-7E6550932917}')!
MonoMethodSignature comment: ''!
!MonoMethodSignature categoriesForClass!External-Data-Unstructured! !
!MonoMethodSignature methodsFor!

= comparand 
	"Answer whether the receiver and the <MonoMethodSignature>, comparand, are considered
	equivalent."

	^self species == comparand species and: [MonoLibrary default monoMetadataSignatureEqual: self sig2: comparand]!

argumentCount
	"Answer the number of parameters in the method described by the receiver."

	^MonoLibrary default monoSignatureGetParamCount: self!

argumentTypes
	"Answer a collection of the argument types in the receiver (instances of <MonoType>)
	in left to right order."

	| iterator type argumentTypes |
	iterator := LPVOID new.
	argumentTypes := OrderedCollection new.
	[(type := MonoLibrary default monoSignatureGetParams: self iter: iterator) isNull] 
		whileFalse: [argumentTypes add: type].
	^argumentTypes!

hash
	"Answer the <Integer> hash value for the receiver."

	^MonoLibrary default monoSignatureHash: self!

returnType
	"Answer the return type of the receiver."

	^MonoLibrary default monoSignatureGetReturnType: self! !
!MonoMethodSignature categoriesFor: #=!comparing!public! !
!MonoMethodSignature categoriesFor: #argumentCount!public! !
!MonoMethodSignature categoriesFor: #argumentTypes!public! !
!MonoMethodSignature categoriesFor: #hash!comparing!public! !
!MonoMethodSignature categoriesFor: #returnType!public! !

MonoObject guid: (GUID fromString: '{7A943122-BE3D-416D-93A3-6E448DEAE0B7}')!
MonoObject comment: ''!
!MonoObject categoriesForClass!External-Data-Unstructured! !
!MonoObject methodsFor!

= comparand 
	"Answer whether the receiver and the <MonoObject>, comparand, are considered equivalent."

	^comparand isMonoObject and: [self Equals: comparand]!

basicFree
	"Private - Free external resources held by the receiver."

	monoLibrary monoGCHandleFree: gcHandle!

copy
	"Answer a copy of the receiver."

	^self shallowCopy postCopy!

doesNotUnderstand: aMessage 
	"Private - Sent to the receiver by the VM when a message sent to the receiver was 
	not implemented by the receiver or its superclasses. In this case we try to find the
	appropiate method for aMessage.

	Implementation Note: We search for the method in a very naive way. Provide a
	more robust implementation that also handles binary messages and fields
	(aka instance/class variables)."

	| method arguments object |
	#todo.
	method := self monoClass methodFor: aMessage.
	arguments := aMessage arguments.
	object := method isClassMethod 
				ifTrue: [method valueWithArguments: arguments]
				ifFalse: [method value: self withArguments: arguments].
	^method returnType isVoid ifTrue: [self] ifFalse: [object]!

domain
	"Answer the <MonoDomain> where the receiver is hosted."

	^monoLibrary monoObjectGetDomain: self!

gcHandle
	"Private - Answer the receiver's GC handle."

	^self gcHandle: true!

gcHandle: aBoolean 
	"Private - Create a GC handle for the receiver.
	If aBoolean is true, the receiver will be pinned so it will not be possible by a moving garbage
	collector to move the object."

	^gcHandle isNil 
		ifTrue: [gcHandle := monoLibrary monoGCHandleNew: self pinned: aBoolean]
		ifFalse: [gcHandle]!

generation
	"Answer the generation number the receiver belongs to."

	^monoLibrary monoGCGetGeneration: self!

hash
	"Answer the <Integer> hash value for the receiver."

	"^monoLibrary monoObjectHash: self"
	^self GetHashCode!

initialize
	"Private - Initialize the receiver by invoking the default argumentless constructor."

	self isValueType ifFalse: [monoLibrary monoRuntimeObjectInit: self]!

initializePointer
	"Private - Mark the receiver as finalizable and create a GC handle for it."

	self notNull 
		ifTrue: 
			[monoLibrary := MonoLibrary default.
			self
				gcHandle;
				beFinalizable]!

isValueType
	"Answer whether the receiver is a ValueType."

	^self monoClass isValueType!

methodArguments
	"Private - Answer the calling method arguments."

	^self class methodArguments!

monoClass
	"Answer a <MonoClass> which is the class of the receiver."

	^monoLibrary monoObjectGetClass: self!

monoType
	"Answer a <MonoType> which is the type of the receiver."

	^self monoClass monoType!

needsFree
	"Private - Answer whether the receiver requires freeing of any external resources."

	^self notNull!

shallowCopy
	"Answer a shallow copy of the receiver."

	^self class fromAddress: (monoLibrary monoObjectClone: self) asParameter!

value
	"Answer the <Object> value represented by the receiver."

	| monoType |
	monoType := self monoType.
	^monoType canBeMarshalled 
		ifFalse: 
			[| class |
			
			[class := Smalltalk at: self monoClass smalltalkClassName.
			class ~~ self class ifTrue: [class fromAddress: self asParameter] ifFalse: [self]] 
					on: NotFoundError
					do: [:ex | self]]
		ifTrue: [(monoType smalltalkClass fromAddress: self yourAddress) asObject]!

yourAddress
	"Answer the address of the receiver's contents."

	^self isValueType ifTrue: [monoLibrary monoObjectUnbox: self] ifFalse: [super yourAddress]! !
!MonoObject categoriesFor: #=!comparing!public! !
!MonoObject categoriesFor: #basicFree!private!realizing/unrealizing! !
!MonoObject categoriesFor: #copy!copying!public! !
!MonoObject categoriesFor: #doesNotUnderstand:!exceptions!private! !
!MonoObject categoriesFor: #domain!public! !
!MonoObject categoriesFor: #gcHandle!private! !
!MonoObject categoriesFor: #gcHandle:!private! !
!MonoObject categoriesFor: #generation!public! !
!MonoObject categoriesFor: #hash!comparing!public! !
!MonoObject categoriesFor: #initialize!private! !
!MonoObject categoriesFor: #initializePointer!initializing!private! !
!MonoObject categoriesFor: #isValueType!public!testing! !
!MonoObject categoriesFor: #methodArguments!private! !
!MonoObject categoriesFor: #monoClass!public! !
!MonoObject categoriesFor: #monoType!public! !
!MonoObject categoriesFor: #needsFree!private!realizing/unrealizing! !
!MonoObject categoriesFor: #shallowCopy!copying!public! !
!MonoObject categoriesFor: #value!converting!public! !
!MonoObject categoriesFor: #yourAddress!accessing!public! !

!MonoObject class methodsFor!

assemblyName
	"Answer the assembly name to which the receiver belongs to."

	^self subclassResponsibility!

box: anObject on: aMonoClass 
	"Answer a new instance of the receiver, of class aMonoClass, whose value is anObject."

	| monoObject |
	monoObject := MonoLibrary default 
				monoValueBox: MonoDomain current
				klass: aMonoClass
				val: anObject asParameter.
	^monoObject initializePointer!

className
	"Answer the receiver's class name."

	^self subclassResponsibility!

classNamespace
	"Answer the receiver's class namespace."

	^self subclassResponsibility!

fromObject: anObject 
	"Answer an instance of the receiver whose value is anObject."

	anObject isMonoObject ifTrue: [^self fromAddress: anObject asParameter].
	^self error: 'Can''t convert ' , anObject displayString , ' to a ' , self name!

isMonoObject
	"Answer whether the receiver is a <MonoObject>."

	^true!

methodArguments
	"Private - Answer the calling method arguments."

	| receiver sender |
	receiver := Processor activeProcess topFrame.
	sender := receiver sender.
	^receiver method selector == sender method selector 
		ifTrue: [sender sender arguments]
		ifFalse: [sender arguments]!

monoClass
	"Answer the <MonoClass> the receiver wraps."

	| assembly |
	monoClass isNil ifFalse: [^monoClass].
	assembly := MonoAssembly named: self assemblyName.
	^monoClass := assembly class: self className namespace: self classNamespace!

new
	"Answer a new instance of the receiver.

	Implementation Note: Initialize the receiver by invoking the default argumentless constructor."

	^self newInstance initialize!

newInstance
	"Private - Answer a new instance of the receiver."

	^self fromAddress: self monoClass newObject asParameter!

typeName
	"Private - Answer the Dolphin external type name for the receiver.
	There is an appropriate built-in type which we can substitute."

	^#object! !
!MonoObject class categoriesFor: #assemblyName!public! !
!MonoObject class categoriesFor: #box:on:!public! !
!MonoObject class categoriesFor: #className!public! !
!MonoObject class categoriesFor: #classNamespace!public! !
!MonoObject class categoriesFor: #fromObject:!instance creation!public! !
!MonoObject class categoriesFor: #isMonoObject!public!testing! !
!MonoObject class categoriesFor: #methodArguments!private! !
!MonoObject class categoriesFor: #monoClass!public! !
!MonoObject class categoriesFor: #new!instance creation!public! !
!MonoObject class categoriesFor: #newInstance!instance creation!private! !
!MonoObject class categoriesFor: #typeName!constants!private! !

MonoProperty guid: (GUID fromString: '{A21FB4E3-3F58-4888-A719-E1321B9A978E}')!
MonoProperty comment: ''!
!MonoProperty categoriesForClass!External-Data-Unstructured! !
!MonoProperty methodsFor!

anyMethod
	"Private - Answer any of the receiver's getter or setter method (whichever is found first)."

	| getter |
	getter := self getterMethod.
	getter isNull ifTrue: [^self setterMethod].
	^getter!

getterMethod
	"Answer the receiver's getter method."

	^MonoLibrary default monoPropertyGetGetMethod: self!

interfaceArgumentCount
	"Answer the argument count of the interface the receiver implements."

	| interfaceFullName startIndex stopIndex interfaceArguments writeStream readStream |
	#todo.	"refactor this method"
	self isGenericInterfaceImplementation ifFalse: [^0].
	interfaceFullName := self interfaceFullName.
	startIndex := interfaceFullName indexOf: $<.
	stopIndex := interfaceFullName lastIndexOf: $>.
	interfaceArguments := interfaceFullName copyFrom: startIndex + 1 to: stopIndex - 1.
	writeStream := String writeStream.
	readStream := interfaceArguments readStream.
	[readStream atEnd] whileFalse: 
			[readStream peek == $< ifTrue: [readStream skipTo: $>].
			readStream atEnd ifFalse: [writeStream nextPut: readStream next]].
	^(writeStream contents subStrings: $,) size!

interfaceFullName
	"Answer the interface full name the receiver implements."

	| propertyName interfaceNameLength |
	propertyName := self name.
	interfaceNameLength := (propertyName lastIndexOf: $. ifAbsent: [^String empty]) - 1.
	^propertyName first: interfaceNameLength!

interfaceName
	"Answer the interface name the receiver implements."

	| interfaceArgumentCount |
	interfaceArgumentCount := self interfaceArgumentCount.
	^self interfaceArgumentCount isZero 
		ifTrue: [self interfacePrefixName]
		ifFalse: [self interfacePrefixName , '_' , interfaceArgumentCount printString]!

interfacePrefixName
	"Private - Answer the interface prefix name the receiver implements."

	^((self interfaceFullName subStrings: $<) first subStrings: $.) last!

interfacePropertyName
	"Answer the interface property name of the receiver."

	self isInterfaceImplementation ifFalse: [^String empty].
	^self interfaceName , '_' , self interfaceSuffixName!

interfaceSuffixName
	"Private - Answer the interface suffix name the receiver implements."

	| propertyName |
	propertyName := self name.
	^propertyName last: propertyName size - self interfaceFullName size - 1!

isClassProperty
	"Answer whether the receiver is a class property."

	^self anyMethod isClassMethod!

isGenericInterfaceImplementation
	"Answer whether the receiver implements a Generic interface method."

	^self name includes: $<!

isInterfaceImplementation
	"Answer whether the receiver implements an interface method."

	^self name includes: $.!

name
	"Answer the receiver's name."

	^MonoLibrary default monoPropertyGetName: self!

propertyClass
	"Answer the <MonoClass> to which the receiver belongs."

	^MonoLibrary default monoPropertyGetParent: self!

setterMethod
	"Answer the receiver's setter method."

	^MonoLibrary default monoPropertyGetSetMethod: self!

smalltalkName
	"Answer the receiver's Smalltalk name."

	^self isInterfaceImplementation ifTrue: [self interfacePropertyName] ifFalse: [self name]! !
!MonoProperty categoriesFor: #anyMethod!private! !
!MonoProperty categoriesFor: #getterMethod!public! !
!MonoProperty categoriesFor: #interfaceArgumentCount!public! !
!MonoProperty categoriesFor: #interfaceFullName!public! !
!MonoProperty categoriesFor: #interfaceName!public! !
!MonoProperty categoriesFor: #interfacePrefixName!private! !
!MonoProperty categoriesFor: #interfacePropertyName!public! !
!MonoProperty categoriesFor: #interfaceSuffixName!private! !
!MonoProperty categoriesFor: #isClassProperty!public!testing! !
!MonoProperty categoriesFor: #isGenericInterfaceImplementation!public!testing! !
!MonoProperty categoriesFor: #isInterfaceImplementation!public!testing! !
!MonoProperty categoriesFor: #name!public! !
!MonoProperty categoriesFor: #propertyClass!public! !
!MonoProperty categoriesFor: #setterMethod!public! !
!MonoProperty categoriesFor: #smalltalkName!public! !

MonoType guid: (GUID fromString: '{B6B58DA5-CD11-44BE-B064-353DE461E058}')!
MonoType comment: ''!
!MonoType categoriesForClass!External-Data-Unstructured! !
!MonoType methodsFor!

= comparand 
	"Answer whether the receiver and the <MonoType>, comparand, are considered equivalent."

	^self species == comparand species and: [MonoLibrary default monoMetadataTypeEqual: self t2: comparand]!

canBeMarshalled
	"Answer whether the receiver can be marshalled."

	^(self smalltalkClass == MonoObject) not!

hash
	"Answer the <Integer> hash value for the receiver."

	^MonoLibrary default monoMetadataTypeHash: self!

isByReference
	"Answer whether the receiver represents a type passed by reference."

	^MonoLibrary default monoTypeIsByRef: self!

isReference
	"Answer whether the receiver is a reference type."

	^MonoLibrary default monoTypeIsReference: self!

isValueType
	"Answer whether the receiver is a ValueType."

	^self type == MONO_TYPE_VALUETYPE!

isVoid
	"Answer whether the receiver is a void type."

	^self type == MONO_TYPE_VOID!

monoClass
	"Answer the <MonoClass> the receiver describes."

	^MonoLibrary default monoClassFromMonoType: self!

name
	"Answer the receiver's name."

	^MonoLibrary default monoTypeGetName: self!

smalltalkClass
	"Answer the Smalltalk class the receiver represents."

	^TypeClasses at: self type ifAbsent: [MonoObject]!

type
	"Private - Answer the receiver's type value."

	^MonoLibrary default monoTypeGetType: self! !
!MonoType categoriesFor: #=!comparing!public! !
!MonoType categoriesFor: #canBeMarshalled!public!testing! !
!MonoType categoriesFor: #hash!comparing!public! !
!MonoType categoriesFor: #isByReference!public!testing! !
!MonoType categoriesFor: #isReference!public!testing! !
!MonoType categoriesFor: #isValueType!public!testing! !
!MonoType categoriesFor: #isVoid!public!testing! !
!MonoType categoriesFor: #monoClass!converting!public! !
!MonoType categoriesFor: #name!public! !
!MonoType categoriesFor: #smalltalkClass!converting!public! !
!MonoType categoriesFor: #type!private! !

!MonoType class methodsFor!

fromName: aString 
	"Answer a new instance of the receiver from its aString name searched in mscorlib."

	^self fromName: aString image: nil!

fromName: aString image: aMonoImage 
	"Answer a new instance of the receiver from its aString name searched in aMonoImage."

	^MonoLibrary default monoReflectionTypeFromName: aString image: aMonoImage!

initialize
	"Private - Initialize the receiver's class variables."

	#todo.
	TypeClasses := IdentityDictionary new.
	TypeClasses
		"at: MONO_TYPE_END put: UndefinedObject;"			"Marks end of a list"
		at: MONO_TYPE_VOID put: VOID;					"A void type (System.Void)"
		at: MONO_TYPE_BOOLEAN put: BOOLEAN;			"A Boolean type (System.Boolean)"
		at: MONO_TYPE_CHAR put: WORD;					"A character type (System.Char)"
		at: MONO_TYPE_I1 put: SBYTE;					"A signed 1-byte integer (System.SByte)"
		at: MONO_TYPE_U1 put: BYTE;					"An unsigned 1-byte integer (System.Byte)"
		at: MONO_TYPE_I2 put: SWORD;					"A signed 2-byte integer (System.Int16)"
		at: MONO_TYPE_U2 put: WORD;					"An unsigned 2-byte integer (System.UInt16)"
		at: MONO_TYPE_I4 put: SDWORD;					"A signed 4-byte integer (System.Int32)"
		at: MONO_TYPE_U4 put: DWORD;					"An unsigned 4-byte integer (System.UInt32)"
		at: MONO_TYPE_I8 put: LARGE_INTEGER;			"A signed 8-byte integer (System.Int64)"
		at: MONO_TYPE_U8 put: ULARGE_INTEGER;			"An unsigned 8-byte integer (System.UInt64)"
		at: MONO_TYPE_R4 put: FLOAT;					"A 4-byte floating point (System.Single)"
		at: MONO_TYPE_R8 put: DOUBLE;					"An 8-byte floating point (System.Double)"
		at: MONO_TYPE_STRING put: MonoString;			"A System.String type"
		"at: MONO_TYPE_PTR put: UndefinedObject;"			"A pointer type modifier"
		"at: MONO_TYPE_BYREF put: UndefinedObject;"		"A reference type modifier"
		"at: MONO_TYPE_VALUETYPE put: UndefinedObject;"	"A value type modifier"
		"at: MONO_TYPE_CLASS put: UndefinedObject;"		"A class type modifier"
		"at: MONO_TYPE_VAR put: UndefinedObject;"			"A class variable type modifier"
		at: MONO_TYPE_ARRAY put: MonoArray;				"A multi-dimensional array type modifier"
		"at: MONO_TYPE_GENERICINST put: UndefinedObject;"	"A type modifier for generic types"
		"at: MONO_TYPE_TYPEDBYREF put: UndefinedObject;"	"A typed reference"
		"at: MONO_TYPE_I put: UndefinedObject;"				"A System.IntPtr type"
		"at: MONO_TYPE_I put: ExternalAddress;"				"A System.IntPtr type"
		"at: MONO_TYPE_U put: UndefinedObject;"			"A System.UIntPtr type"
		"at: MONO_TYPE_U put: ExternalAddress;"			"A System.UIntPtr type"
		"at: MONO_TYPE_FNPTR put: UndefinedObject;"		"A pointer to a function"
		"at: MONO_TYPE_OBJECT put: UndefinedObject;"		"A System.Object type"
		"at: MONO_TYPE_OBJECT put: MonoObject;"			"A System.Object type"
		at: MONO_TYPE_SZARRAY put: MonoArray			"A single-dimensional, zero lower-bound array type modifier"
		"at: MONO_TYPE_MVAR put: UndefinedObject;"			"A method variable type modifier"
		"at: MONO_TYPE_CMOD_REQD put: UndefinedObject;"	"A C language required modifier"
		"at: MONO_TYPE_CMOD_OPT put: UndefinedObject;"	"A C language optional modifier"
		"at: MONO_TYPE_INTERNAL put: UndefinedObject;"		"Implemented within the CLI"
		"at: MONO_TYPE_MODIFIER put: UndefinedObject;"		"ORed with following element types"
		"at: MONO_TYPE_SENTINEL put: UndefinedObject;"		"Sentinel for vararg method signature"
		"at: MONO_TYPE_PINNED put: UndefinedObject;"		"A local variable that points to a pinned object"
		"at: MONO_TYPE_ENUM put: UndefinedObject"			"An enumeration"
		"at: MONO_TYPE_ENUM put: PoolConstantsDictionary"	"An enumeration"! !
!MonoType class categoriesFor: #fromName:!instance creation!public! !
!MonoType class categoriesFor: #fromName:image:!instance creation!public! !
!MonoType class categoriesFor: #initialize!initializing!private! !

MonoVTable guid: (GUID fromString: '{0686DE08-2A67-4691-841F-672F29CC91C4}')!
MonoVTable comment: ''!
!MonoVTable categoriesForClass!External-Data-Unstructured! !
!MonoVTable methodsFor!

initialize
	"Private - Initialize the receiver."

	MonoLibrary default monoRuntimeClassInit: self! !
!MonoVTable categoriesFor: #initialize!initializing!private! !

MonoArray guid: (GUID fromString: '{32586FA1-6B20-4618-A0B0-E69EAEC43BCB}')!
MonoArray comment: ''!
!MonoArray categoriesForClass!External-Data-Unstructured! !
!MonoArray methodsFor!

= comparand 
	"Answer whether the receiver and the <Object> argument, comparand,
	are considered equivalent - that is they are of the same class and size, and
	contain eqivalent elements that are in the same sequence.

	Implementation Note: We perform a series of tests of increasing slowness, culminating
	in an element-by-element equality test."

	| size |
	self == comparand ifTrue: [^true].
	self species == comparand species ifFalse: [^false].
	size := self size.
	size = comparand size ifFalse: [^false].
	1 to: size do: [:i | (self at: i) = (comparand at: i) ifFalse: [^false]].
	^true!

addressOfElementAt: anInteger 
	"Private - Answer the <ExternalAddress> of the element at anInteger index in the receiver."

	^monoLibrary 
		monoArrayAddrWithSize: self
		size: self elementSize
		idx: anInteger - 1!

asArray
	"Answer an <Array> containing the constituent elements of the receiver."

	^self collect: [:each | each]!

asByteArray
	"Answer a <ByteArray> containing the constituent elements of the receiver."

	| size byteArray |
	size := self size.
	byteArray := ByteArray new: size.
	1 to: size do: [:index | byteArray at: index put: (self at: index)].
	^byteArray!

asMonoArray
	"Answer a <MonoArray> containing the same elements as the receiver.
	In this case no conversion is required."

	^self!

at: anInteger 
	"Answer the element of the receiver at the specified index.
	Raise an error if the index if out of bounds."

	| externalAddress object elementMonoClass |
	(anInteger < 1 or: [anInteger > self size]) ifTrue: [^self errorSubscriptBounds: anInteger].
	externalAddress := self addressOfElementAt: anInteger.
	object := self elementFromAddress: externalAddress.
	elementMonoClass := self elementMonoClass.
	^((object isMonoObject and: [elementMonoClass isValueType]) 
		ifTrue: [MonoObject box: object on: elementMonoClass]
		ifFalse: [object]) asObject!

at: index ifAbsent: exceptionBlock 
	"Answer an <Object> which is the element of the receiver  at the specified index.
	If the index is out of bounds answer the result of evaluating the <niladicValuable>
	exceptionBlock."

	^[self at: index] on: BoundsError do: [:ex | exceptionBlock value]!

at: anInteger put: anObject 
	"Set the element of the receiver at the specified index.
	Raise an error if the index is out of bounds."

	| externalAddress element |
	#todo.	"refactor this method"
	(anInteger < 1 or: [anInteger > self size]) ifTrue: [^self errorSubscriptBounds: anInteger].
	externalAddress := self addressOfElementAt: anInteger.
	element := self elementClass fromObject: anObject.
	self elementMonoClass monoType isReference 
		ifTrue: 
			[monoLibrary 
				monoGCWBarrierSetArrayRef: self
				slotPtr: externalAddress
				value: element yourAddress]
		ifFalse: 
			[| byteArray |
			byteArray := ByteArray fromAddress: element yourAddress length: self elementSize.
			externalAddress 
				replaceFrom: 1
				to: byteArray size
				with: byteArray
				startingAt: 1].
	^anObject!

collect: transformer 
	"Evaluate the <monadicValuable> argument, transformer, for each of the 
	receiver's elements in the order defined by the receiver's implementation of #do:.
	Answer a new collection like the receiver (i.e. of the same species but not
	necessarily the exact same class) containing the values returned by transformation
	block.

	Implementation Note: By making use of the special instance creation method 
	#copyLikeOfSize: we can avoid using a WriteStream here."

	| answer |
	answer := self copyLikeOfSize: self size.
	self keysAndValuesDo: [:i :each | answer at: i put: (transformer value: each)].
	^answer!

copyLikeOfSize: anInteger 
	"Private - Answer a new collection of the same species as the receiver but with
	anInteger nil elements - i.e. not just with sufficient capacity for, but actually holding, 
	anInteger elements."

	^self species ofSize: anInteger!

dimensions
	"Answer the number of dimensions in the receiver."

	^self rank!

do: operation 
	"Evaluate the <monadicValuable> argument, operation, for each of the elements of the 
	receiver. Answers the receiver. The elements are evaluated in index order."

	1 to: self size do: [:i | operation value: (self at: i)]!

do: operation separatedBy: separator 
	"Evaluate the <monadicValuable> argument, operation, for each of the 
	receiver's elements, interspersed with evaluations of the <niladicValuable>
	argument, separator. The separator is first evaluated after the first
	element, and is not evaluated after the last element (i.e. it is not evaluated
	at all if there are less than two elements)."

	| sep |
	sep := [sep := separator].	"Switch to the real separator after first eval."
	self do: 
			[:each | 
			sep value.
			operation value: each]!

elementClass
	"Answer the class of Smalltalk object used to represent the elements of the receiver."

	^elementClass ifNil: [elementClass := self elementMonoClass monoType smalltalkClass]!

elementClass: aClass 
	"Private - Set the class of Smalltalk object used to represent the elements of the receiver."

	elementClass := aClass!

elementFromAddress: anExternalAddress 
	"Private - Answer a receiver's element from anExternalAddress."

	^self elementClass fromAddress: (self elementMonoClass monoType isReference 
				ifTrue: [anExternalAddress addressAtOffset: 0]
				ifFalse: [anExternalAddress])!

elementMonoClass
	"Private - Answer the <MonoClass> used to represent the elements of the receiver."

	^self monoClass elementClass!

elementSize
	"Answer the size in bytes of the indidividual elements of the receiver."

	^self monoClass elementSize!

hash
	"Answer the <Integer> hash value for the receiver.

	Implementation Note: This is not a terribly good hash function (in particular it
	exhibits very poor temporal stability), but it is quick, and alternatives involve expensive 
	iteration through the receiver's elements."

	| size |
	(size := self size) == 0 ifTrue: [^17171].
	^size + (self at: 1) hash + (self at: size) hash!

keysAndValuesDo: operation 
	"Evaluate the <dyadicValuable>, operation, for each element of the receiver
	with the index of that element and the element itself as the arguments.

	Implementation Note: Subclasses should override #from:to:keysAndValuesDo: rather 
	than this method, unless they have a slow implementation of #size."

	self 
		uncheckedFrom: 1
		to: self size
		keysAndValuesDo: operation!

length
	"Answer the number of elements in the receiver."

	^monoLibrary monoArrayLength: self!

rank
	"Answer the receiver's rank (the number of dimensions)."

	^self monoClass rank!

shallowCopy
	"Answer a shallow copy of the receiver."

	^self class fromAddress: (monoLibrary monoArrayClone: self) asParameter!

size
	"Answer the number of elements in the receiver."
	
	^self length!

species
	"Answer the class of object to be used when copying the receiver."

	^Array!

uncheckedFrom: startInteger to: stopInteger keysAndValuesDo: aDyadicValuable 
	"Private - Evaluate the <dyadicValuable> argument for each element of the receiver in the
	specified inclusive range, with the element and its index as respectively the second and
	first arguments. No bounds checks need be performed since the caller has established that
	the start and stop indices are in bounds.

	Implementation Note: Subclasses should override this method in order to replace all #do:
	family enumerators (#do:, #keysAndValuesDo:, #from:to:do:, and, of course,
	#from:to:keysAndValuesDo:). Subclasses may also want to override this if they have a slow
	implementation of the random accessor method #at:, but can be more efficiently accessed
	serially (e.g. a singly Linked List), or if they have to calculate their size using
	#countElements (which would cause an infinite recursion as it uses #do:, which uses #size,
	...)."

	startInteger to: stopInteger do: [:i | aDyadicValuable value: i value: (self at: i)]!

value
	"Answer the <Object> value represented by the receiver (an <Array> of <Objects>)."

	#todo.	"add support for multidimensional arrays"
	self dimensions > 1 ifTrue: [^self].
	^self elementClass == BYTE ifTrue: [self asByteArray] ifFalse: [self asArray]! !
!MonoArray categoriesFor: #=!comparing!public! !
!MonoArray categoriesFor: #addressOfElementAt:!private! !
!MonoArray categoriesFor: #asArray!converting!public! !
!MonoArray categoriesFor: #asByteArray!converting!public! !
!MonoArray categoriesFor: #asMonoArray!converting!public! !
!MonoArray categoriesFor: #at:!accessing!public! !
!MonoArray categoriesFor: #at:ifAbsent:!accessing!public! !
!MonoArray categoriesFor: #at:put:!accessing!public! !
!MonoArray categoriesFor: #collect:!enumerating!public! !
!MonoArray categoriesFor: #copyLikeOfSize:!copying!private! !
!MonoArray categoriesFor: #dimensions!accessing!public! !
!MonoArray categoriesFor: #do:!enumerating!public! !
!MonoArray categoriesFor: #do:separatedBy:!enumerating!public! !
!MonoArray categoriesFor: #elementClass!accessing!public! !
!MonoArray categoriesFor: #elementClass:!accessing!private! !
!MonoArray categoriesFor: #elementFromAddress:!private! !
!MonoArray categoriesFor: #elementMonoClass!accessing!private! !
!MonoArray categoriesFor: #elementSize!accessing!public! !
!MonoArray categoriesFor: #hash!comparing!public! !
!MonoArray categoriesFor: #keysAndValuesDo:!enumerating!public! !
!MonoArray categoriesFor: #length!accessing!public! !
!MonoArray categoriesFor: #rank!accessing!public! !
!MonoArray categoriesFor: #shallowCopy!copying!public! !
!MonoArray categoriesFor: #size!accessing!public! !
!MonoArray categoriesFor: #species!constants!public! !
!MonoArray categoriesFor: #uncheckedFrom:to:keysAndValuesDo:!enumerating!private! !
!MonoArray categoriesFor: #value!converting!public! !

!MonoArray class methodsFor!

fromArray: anArray 
	"Answer a new instance of the receiver from anArray."

	| element |
	#todo.
	element := anArray first.
	element isString ifTrue: [^self fromArray: anArray elementClass: MonoString].
	element isMonoObject ifTrue: [^self fromArray: anArray elementClass: MonoObject].
	(element == true or: [element == false]) ifTrue: [^self fromArray: anArray elementClass: BOOLEAN]!

fromArray: anArray elementClass: elementClass 
	"Answer a new instance of the receiver from anArray that contains elements of type,
	elementClass."

	| size monoArray |
	size := anArray size.
	monoArray := self length: size elementClass: elementClass.
	1 to: size do: [:index | monoArray at: index put: (anArray at: index)].
	^monoArray!

fromByteArray: aByteArray 
	"Answer a new instance of the receiver from aByteArray."

	^self fromArray: aByteArray elementClass: BYTE!

fromObject: anObject 
	"Answer an instance of the receiver whose value is anObject."

	| class |
	#todo.
	class := anObject class.
	class == self ifTrue: [^self fromAddress: anObject asParameter].
	class == ByteArray ifTrue: [^self fromByteArray: anObject].
	class == Array ifTrue: [^self fromArray: anObject].
	^self error: 'Can''t convert ' , anObject displayString , ' to a ' , self name!

length: anInteger elementClass: elementClass 
	"Answer a new vector (single dimensioned) <MonoArray> that contains anInteger
	elements of type, elementClass."

	| monoArray |
	monoArray := MonoLibrary default 
				monoArrayNew: MonoDomain current
				eclass: (self monoClassFor: elementClass)
				n: anInteger.
	monoArray elementClass: elementClass.
	^monoArray initializePointer!

monoClass
	"Answer the <MonoClass> the receiver wraps."

	^MonoClass array!

monoClassFor: aClass 
	"Private - Answer the <MonoClass> aClass wraps."

	^aClass isMonoObject 
		ifTrue: [aClass monoClass]
		ifFalse: 
			[[(Message selector: aClass typeName) value: MonoClass]
				on: MessageNotUnderstood
				do: [:ex | self error: 'Invalid element class: ' , aClass name]]!

typeName
	"Private - Answer the Dolphin external type name for the receiver.
	There is an appropriate built-in type which we can substitute."

	^#array! !
!MonoArray class categoriesFor: #fromArray:!instance creation!public! !
!MonoArray class categoriesFor: #fromArray:elementClass:!public! !
!MonoArray class categoriesFor: #fromByteArray:!instance creation!public! !
!MonoArray class categoriesFor: #fromObject:!instance creation!public! !
!MonoArray class categoriesFor: #length:elementClass:!instance creation!public! !
!MonoArray class categoriesFor: #monoClass!public! !
!MonoArray class categoriesFor: #monoClassFor:!private! !
!MonoArray class categoriesFor: #typeName!constants!private! !

MonoString guid: (GUID fromString: '{D6394497-322B-4710-B216-C89616722104}')!
MonoString comment: ''!
!MonoString categoriesForClass!External-Data-Unstructured! !
!MonoString methodsFor!

= comparand 
	"Answer whether the receiver and the argument, comparand, are considered equivalent."

	"^self species == comparand species and: [monoLibrary monoStringEqual: self s2: comparand]."
	^comparand isString and: [self asString = comparand asString]!

asMonoString
	"Answer a <MonoString> containing the same elements as the receiver.
	In this case no conversion is required."

	^self!

asObject
	"Answer the <Object> value represented by the receiver."

	^self asString!

asString
	"Answer a byte string representation for the receiver."

	^self value asString!

asUnicodeString
	"Answer a wide character string representation of the receiver."

	^self value!

hash
	"Answer the <Integer> hash value for the receiver."

	^monoLibrary monoStringHash: self!

isString
	"Answer whether the receiver represents a <String>."

	^true!

size
	"Answer the number of characters in the receiver."

	^monoLibrary monoStringLength: self!

species
	"Answer the class of object to be used when copying the receiver."

	^UnicodeString!

value
	"Answer a wide character string representation of the receiver."

	| externalAddress unicodeString |
	externalAddress := monoLibrary monoStringToUTF16: self.
	unicodeString := self species fromAddress: externalAddress.
	monoLibrary monoFree: externalAddress.
	^unicodeString! !
!MonoString categoriesFor: #=!comparing!public! !
!MonoString categoriesFor: #asMonoString!converting!public! !
!MonoString categoriesFor: #asObject!converting!public! !
!MonoString categoriesFor: #asString!converting!public! !
!MonoString categoriesFor: #asUnicodeString!converting!public! !
!MonoString categoriesFor: #hash!comparing!public! !
!MonoString categoriesFor: #isString!public!testing! !
!MonoString categoriesFor: #size!accessing!public! !
!MonoString categoriesFor: #species!constants!public! !
!MonoString categoriesFor: #value!converting!public! !

!MonoString class methodsFor!

empty
	"Answer an empty instance of the receiver."

	^self fromString: ''!

fromObject: anObject 
	"Answer an instance of the receiver whose value is anObject."

	| class |
	class := anObject class.
	class == self ifTrue: [^self fromAddress: anObject asParameter].
	(class == String or: [class == Symbol]) ifTrue: [^self fromString: anObject].
	class == UnicodeString ifTrue: [^self fromUnicodeString: anObject].
	^self error: 'Can''t convert ' , anObject displayString , ' to a ' , self name!

fromString: aString 
	"Answer a new instance of the receiver from aString."

	| monoString |
	monoString := MonoLibrary default monoStringNew: MonoDomain current text: aString.
	^monoString initializePointer!

fromUnicodeString: aUnicodeString 
	"Answer a new instance of the receiver from aUnicodeString."

	| monoString |
	monoString := MonoLibrary default monoStringFromUTF16: aUnicodeString.
	^monoString initializePointer!

monoClass
	"Answer the <MonoClass> the receiver wraps."

	^MonoClass string!

new
	"Answer a new instance of the receiver."

	^self empty!

typeName
	"Private - Answer the Dolphin external type name for the receiver.
	There is an appropriate built-in type which we can substitute."

	^#string! !
!MonoString class categoriesFor: #empty!instance creation!public! !
!MonoString class categoriesFor: #fromObject:!instance creation!public! !
!MonoString class categoriesFor: #fromString:!instance creation!public! !
!MonoString class categoriesFor: #fromUnicodeString:!instance creation!public! !
!MonoString class categoriesFor: #monoClass!public! !
!MonoString class categoriesFor: #new!instance creation!public! !
!MonoString class categoriesFor: #typeName!constants!private! !

"Binary Globals"!

