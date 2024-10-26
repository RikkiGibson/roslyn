# Lifetime Analysis
This document describes the compiler's system for analyzing safe usage of managed references in code. The fundamental thing we are trying to do is forbid use of a given `ref` in any program location where its *referent* is not *live*.

Note that this is not a flow analysis. This means, for example, that a variable's lifetime is fixed when it is declared, rather than potentially being different at different locations in a method.

## Terms
- `ref`: A managed reference to a variable, usually made using a `ref`/`in`/`out` keyword in an argument or assignment position. Corresponds to the CLR concept of a *managed pointer* or *byref*.
- writable `ref`: refers only to refs made using the `ref` keyword.
- `ref readonly`: a `ref` whose referent can be read but not written.
- `ref struct`: a struct which is assumed to contain one or more `ref` fields.
- **referent**: the variable which a `ref` is referring to.
- **lifetime**: the program region in which the **referent** of a given `ref` is **live**.
- **live**: means that a variable has storage allocated for it at a given program location. A variable is live at a given program location if:
    - it is on the heap, or,
    - it is a local or parameter in scope, or,
    - it is contained in a `stackalloc` buffer in the current stack frame, or,
    - it is any of the above in the enclosing stack frame in the current thread.

## Theory
In order to achieve a clear, concise, and "verifiably" correct design, we define a theory using types to represent lifetimes. Lifetime types, type parameters, type arguments, and conversions define in what ways `ref`s are permitted to flow through the program.

### Ref lifetime
A `ref` itself has exactly one lifetime. This is represented using variations on the same type argument form, depending on the readability/writability of the `ref`.
- `ref<$a> readonly`
- `in<$a>`
- `out<$a>`

### Lifetime parameters
`ref struct` declarations can declare lifetime parameters, in order to allow the consumer of the `ref struct` to specify the lifetimes of `ref` fields within:  
`ref struct Span<T, $a> { ref<$a> T reference; ... }`.

Methods can also declare lifetime parameters in order to denote the lifetimes of the references used as arguments. For example:

```cs
void M<$a, $b>(ref<$a> Span<int, $b> param) where $b : $a { ... }

int i = 0;
Span<int, $M_local0> span = new Span<int, $M_local0>(ref i);
```

### Lifetime constraints
Lifetime parameters can be related to each other by constraints: `where $b : $a` means that lifetime `$b` is *wider than or equal to* lifetime `$a`. This is analogous to a type parameter constraint `where TDerived : TBase`.

### Well-known lifetimes
- `$heap` is the "widest" lifetime. When a `ref` has lifetime `$heap`, its referent is always live.
    - `where $heap : $a` holds for all lifetimes `$a`.
- `$M_local0` is the "top level" lifetime of the current method `M`. `ref`s to parameters and top-level local variables in `M` have this lifetime.
- `$M_localN` is the local scope at some depth of nesting (given by `N`) in the current method. `refs` to local variables at the depth of nesting given by `N` have this lifetime.
    - `where $M_local(N-1) : $M_localN` holds for all scope depths `N`.

A few concrete example usages:

```cs
void M()
{
    int i = 0;
    ref<$M_local0> ri = ref i;

    {
        int j = 0;
        ref<$local_1> rj = ref j;
    }
}
```

```cs
class C
{
    int F;

    void M()
    {
        Span<int, $heap> span = new Span<int, $heap>(ref this.F);
    }
}
```

Using a well-known lifetime as an argument for a lifetime parameter is the key mechanism which allows refs to locals and parameters to flow across method boundaries.

### Conversions
An implicit conversion exists from `ref<$b>` to `ref<$a>` when `$b : $a` (`$b` is *wider than or equal to* `$a`).
- `ref<$a>` is also convertible to `ref<$a> readonly`.
- `ref<$b> readonly` is also convertible to `ref<$a> readonly`.

### Nested lifetimes
Lifetimes can be nested. For example, `ref<$a> Span<int, $b>`. Given `ref<$a>`, all lifetimes `$b` in the *referent* must meet the implicit constraint `where $b : $a`. Otherwise, an error occurs.
- This is equivalent to saying that a **wider** `ref`'s referent must not contain a **narrower** `ref`.

Nested lifetimes within a writable `ref` are **invariant**. For example, even when `$c: $b : $a`, no conversion exists from `ref<$a> Span<int, $c>` to `ref<$a> Span<int, $b>`. This is analogous to disallowing conversion from `List<string>` to `List<object>`.

Nested lifetimes within a `ref readonly` are **covariant**. For example, when `$c : $b : $a`, it is permitted to convert from `ref<$a> readonly Span<int, $c>` to `ref<$a> readonly Span<int, $b>`. This is analogous to permitting conversion from `IEnumerable<string>` to `IEnumerable<object>`.

(Note that in theory, lifetimes can also be nested through `ref struct` types themselves to arbitrary depths. Since C# does not support ref fields of ref struct type, and possibly never will, we do not specify this precisely. But, it's not all that different from the `ref` case itself.)

### Lifetime inference
When a method with lifetime parameters is invoked, a *lifetime parameter inference* is performed based on the method's arguments.

TODO2: imitate type inference spec language a bit more. Essentially, for each lifetime parameter, we choose the *narrowest* of the lifetimes coming from the method arguments corresponding to that parameter. This is equivalent to choosing the least-derived type for a type argument inference (i.e. upper bound inference).

TODO2: what we want is language describing how ordinary arguments and parameters of various forms, contribute bounds to certain lifetime parameters, depending on the shape of those arguments and parameters.

```cs
void M1<$a>(ref<$a> r1, ref<$a> r2) { }

void M2()
{
    int i = 0;
    {
        int j = 0;
        M1(ref i, ref j); // Infer '$M2_local1'
    }
}
```

This is analogous to an ordinary type argument inference e.g. `void M1<T>(T t1, T t2); M1("a", new object())`, resulting in `object`.


## Translation from actual C# code

We define a translation of actual C# code to the theory outlined above.

### Ref struct declarations

A ref struct declaration has a single lifetime parameter, which is implicitly used as an argument to all `ref` fields in the struct.

```cs
// before
ref struct Span<T>
{
    ref T reference;
    int length;
}

// after
ref struct Span<T, $a>
{
    ref<$a> T reference;
    int length;
}
```

### Method declarations

A method declaration `M` has 3 lifetime parameters:
- `$cc`: "caller context". The `ref` can be returned, and can also be written to a referent of a `ref` parameter.
- `$ro`: "return only". The `ref` may be returned, but cannot be written to a referent of a `ref` parameter.
- `$fm`: "function-member". The `ref` cannot be returned.

The following constraints are implicitly defined:
- `where $cc : $ro`
- `where $ro : $fm`

Within the implementation of method `M`, an *identity conversion* is permitted between `$fm` and `$M_local0`. While this is not formally sound, the caller will never observe any refs with lifetime `$fm`.

Given a method `M`, the lifetime of a `ref` in a parameter or return is determined as follows:
- If the `scoped` modifier is used, then the lifetime is `$fm`.
    - e.g. `scoped ref int param` translates to `ref<$fm> int param`.
- If `[UnscopedRef]` is used on the parameter, then the lifetime is `$cc`.
    - e.g. `[UnscopedRef] ref int param` translates to `ref<$cc> int param`.

The following translation is performed on the original method parameters:
- A by-value, non-`ref struct` parameter is translated without modification. For example, `object param`.
- A by-`ref`/`in`, non-`ref struct`, parameter is translated as follows:
    - `ref object param` is translated to `ref<$ro> object param`.
    - `in object param` is translated to `in<$ro> object param`.
- A by-`ref`/`in`, `ref struct` parameter is translated as follows:
    - `ref RefStruct param` is translated to `ref<$
- A parameter of the form `ref RefStruct<T> param` is translated to `ref<$ro> RefStruct<T, $cc> param`.

TODO2: spec other kinds of invocations.

## Implementation sketch

- Bound tree visitor
- `struct Lifetime`
- `static bool CheckConversion(ExpressionWithLifetime source, Lifetime target, DiagnosticBag diagnostics)`
- `struct ExpressionWithLifetimes`
