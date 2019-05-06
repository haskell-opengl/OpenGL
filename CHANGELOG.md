3.0.3.0
-------
* Handle MonadFail proposal.

3.0.2.2
-------
* Relaxed upper version bound for `containers`.

3.0.2.1
-------
* Relaxed upper version bound for `OpenGLRaw`.

3.0.2.0
-------
* Added support for S3_s3tc, EXT_texture_compression_s3tc, ARB_texture_float, and EXT_packed_depth_stencil extensions.

3.0.1.0
-------
* Added `Uniform` instances for `GLmatrix`, `Vertex1`, `Vector1`, `Vector2`, `Vector3`, and `Vector4`.
* Unbreak `Uniform` instances for `GLint`, `GLuint` and `Gldouble`.
* Relaxed upper version bound for `OpenGLRaw`.

3.0.0.2
-------
* Removed redundant constraints.

3.0.0.1
-------
* Relaxed upper version bound for `OpenGLRaw`.

3.0.0.0
-------
* Depend on new `OpenGLRaw` and `GLURaw` packages.

2.13.2.1
--------
* Relaxed upper version bound for `transformers`.

2.13.2.0
--------
* Implement Uniform instances for `GLint`, `GLuint`, `GLfloat`, and `GLdouble`.

2.13.1.1
--------
* Aftermath for the `glClearNamedFramebufferfi` chaos in the OpenGL registry,
  see the corresponding
  [issue](https://www.khronos.org/bugzilla/show_bug.cgi?id=1394) on Khronos.

2.13.1.0
--------
* Added `extensionSupported`.
* Relaxed upper version bound for OpenGLRaw.
* Added CHANGELOG.md to distribution.

2.13.0.0
--------
* Added missing drawing commands using vertex arrays and some related types.
* Added missing whole framebuffer operations.
* Added getters for `stencilMaskSeparate`, `stencilFuncSeparate`, and `stencilOpSeparate`, making them full-blown `StateVar`s.
* Added `patchDefaultInnerLevel` and `patchDefaultOuterLevel` to control the default tessellation levels.
* Added `ContextLost` constructor to `ErrorCategory` type.
* Added `SeparateAttribs` and deprecated `SeperateAttribs`, fixing a spelling mistake.
