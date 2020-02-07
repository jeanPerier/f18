# Character Expressions Lowering

- Inputs: `Expr<SomeCharacter>`
- Output: `mlir::Value` of type `fir.boxchar` or `fir.box`. In the interface description,
these values will be called `charValue`.

TODO: Add `fir.char<kind>` as output for compile time constant length `1` ?
TODO: Do we want to optimize box/unbox operations in expression trees ?

The reference in `fir.box_char` must be of type `fir.ref<fir.array<len x shape x fir.char<kind>>>` (for scalar `shape` is not present).
When the length is a compile time constant, it will be explicit in the type, else, it will be `?`.
At runtime, the atcual lenght will always be accessible in the lenght filed of `fir.box_char`.

TODO: Specify more regarding `fir.box` for character alloacatbles, pointers, deffered array and such.
TODO: how could we propagate the "read-only, temp, or writable" information ?

``
For all character operations, two "pseudo-fir" (*) operations will be created:
 - one to compute the result length
 - the other to compute the result the pseudo-fir operation
 
The pseudo-fir operations:
## Evaluate Character Operation | fir pseudo op |
### Constant<Result>

### `ArrayConstructor<Result>`
TODO

### `Designator<Result>`
- Scalar:
 A scalar character designator can only designate a contiguous entity, so it can always be lowered to a `fir.box_char`.
 This implies computing the length and creating a reference to the start of the string.

- References that are Array:
Simply contiguous array can be put in a `fir.box_char` where the reference is a `fir.ref<fir.array<len x shape x fir.char>>`.
Question: Do we need to compute/propagate the dynamic shape if not compile-time constant ?
(I think this questions apply to array refrences in general).
References that are not simply contiguous will be put in `fir.box` (TODO: specify this more).


### `FunctionRef<Result>` (user functions)
Treated as part of function calls lowering.

### `FunctionRef<Result>` (intrinsic functions)
- `MIN`, `MAX`:
See `Extremum<Result>`.
- `CHAR`, `ACHAR`:
Do we really need to allocate this ?
This can be directly lowered to `fir.convert`
- `ADJUSTL`, `ADJUSTR`:
Implemented by: `adjust(CharValue res, CharValue input, Attribute isLeft)`
Length computation is obvious (= argument lenght), so no operation to compute it.
- `NEW_LINE`:
Do we really need to allocate this ?
This can be directly lowered to `mlir.constant` + `fir.convert`.
- `TRIM`:
For this one, the result lengths is smaller than the input, but computing it is linear on the length,
so we may want to skip this and just provide it a result with enough space.
`CharValue trim(CharValue res, CharValue input)`.
Constraints: `res` must have sufficent length (lentgh of input is enough). If `res` length is not sufficient,
`trim` will stop when reaching the lenght of `res`.
The actual result length is returned in `CharValue`, its memory reference is the same as the one in `res`.
- `MAXVAL`, `MINVAL`:
The result length is trivial (length of array argument). TODO: Decide of an interface based on following observations
(The main question is can we save the temp for this one ?):
   - If DIM is not present, and the array is not empty, the resuly is a scalar reference so we can save the buffer.
   - If DIM is present and the array is not empty, can we always produce a remapping in a `fir.box` ?
   - If the array is empty, we need a temp buffer to return characters filled with CHAR(0).

- `REPEAT(s, n)`

TODO: more.

The length is quite obvious (n*len(s)), but we might want to wrap this in a repeatLength
to easily add runtime checks that the new size does not overflow.


### `Parentheses<Result>`
TODO: Do we need to make a copy ? in `foo((s))`, I think we would need to copy `s`,
but in `s1 // (s2 // s3)` it would be a bit stupid to do so. If we knew whether `charValue` are created temp vs designator,
we could save this. 

### `Convert<Result>`
// Fortran doesn't have conversions between kinds of CHARACTER apart from
// assignments, and in those the data must be convertible to/from 7-bit ASCII.
TODO: How to do that. Is it just a raw casts or does it involve going to/from UTF-8 ?

### `Concat<KIND>`
`Concat(charValue, llvm::ArrayRef<charValue>)`.

### `Extremum<Result>`
`Extremum(charValue, llvm::ArrayRef<charValue>, boolAttribute isMax)`

### `SetLength<KIND>`
setLength
Padding or truncating.
Do we need it ?


## Character Expression Consumers
### Character assignement
A `assign(charValue rhs, charValue lhs)` pseudo-fir operation will be available to hanlde scalar character assignement
according to Fortran rules.
It is the only character pseudo-op that may allocate a temporary to support cases where rhs and lhs overlap.

### User function arguments
The main question here is to handle cases when we need to copy the data or not.

### IO
Same as function arguments ?


(*): "pseudo-fir" operations are defined as fir operations but they are not implemented in FIR.
Instead, there is a function in the lowering code with the same interface as the fir operation builder would have had.
This functions directly lowers the operations to existing fir operations.
The idea is to limit the number of actual fir operation while making it easy to later
promote some pseudo-operations to actual fir operations.
