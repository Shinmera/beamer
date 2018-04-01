#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(defun hex->vec (hex)
  (vec4 (/ (ldb (byte 8 16) hex) 255)
        (/ (ldb (byte 8 8) hex) 255)
        (/ (ldb (byte 8 0) hex) 255)
        1))

(defparameter *monokai-theme*
  (list :text (hex->vec #xf8f8f2)
        :comment (hex->vec #x75715e)
        :keyword (hex->vec #x66d9ef)
        :construct (hex->vec #xf92672)
        :string (hex->vec #xe6db74)))

(defparameter *glsl-tokens*
  (list :comment "//.*(\\n|$)"
        :string "\"[^\"]*\""
        :construct "while|using|typedef|this|template|switch|subroutine|struct|sizeof|return|restrict|out|output|notinline|namespace|layout|input|inout|inline|in|interface|invariant|if|goto|for|external|extern|enum|else|do|discard|default|continue|cont|class|cast|case|break|writeonly|volatile|union|uniform|resource|readonly|public|precision|patch|partition|attribute|asm|active|const|common|coherent"
        :keyword "void|vec4|vec3|vec2|varying|uvec4|uvec3|uvec2|usamplerCubeArray|usamplerCube|usamplerBuffer|usampler3D|usampler2DRect|usampler2DMSArray|usampler2DMS|usampler2DArray|usampler2D|usampler1DArray|usampler1D|unsigned|uint|uimageCubeArray|uimageCube|uimageBuffer|uimage3D|uimage2DRect|uimage2DMSArray|uimage2DMS|uimage2DArray|uimage2D|uimage1DArray|uimage1D|true|superp|static|smooth|smapler2DRectShadow|short|shared|samplerCubeShadow|samplerCubeArrayShadow|samplerCubeArray|samplerCube|samplerBuffer|sampler3DRect|sampler3D|sampler2DShadow|sampler2DRect|sampler2DMSArray|sampler2DMS|sampler2DArrayShadow|sampler2DArray|sampler2D|sampler1DShadow|sampler1DArrayShadow|sampler1DArray|sampler1D|sample|precise|noperspective|mediump|mat4x4|mat4x3|mat4x2|mat4|mat3x4|mat3x3|mat3x2|mat3|mat2x4|mat2x3|mat2x2|mat2|lowp|long|ivec4|ivec3|ivec2|isamplerCubeArray|isamplerCube|isamplerBuffer|isampler3D|isampler2DRect|isampler2DMSArray|isampler2DMS|isampler2DArray|isampler2D|isampler1DArray|isampler1D|int|imageCubeArray|imageCube|imageBuffer|image3D|image2DRect|image2DMSArray|image2DMS|image2DArray|image2D|image1DArray|image1D|iimageCubeArray|iimageCube|iimageBuffer|iimage3D|iimage2DRect|iimage2DMSArray|iimage2DMS|iimage2DArray|iimage2D|iimage1DArray|iimage1D|hvec4|hvec3|hvec2|highp|half|fvec4|fvec3|fvec2|float|flat|fixed|filter|false|dvec4|dvec3|dvec2|double|dmat4x4|dmat4x3|dmat4x2|dmat4|dmat3x4|dmat3x3|dmat3x2|dmat3|dmat2x4|dmat2x3|dmat2x2|dmat2|centroid|bvec4|bvec3|bvec2|buffer|bool|atomic_uint"))

(defparameter *lisp-tokens*
  (list :comment ";.*(\\n|$)"
        :string "\"[^\"]*\""
        :construct "\\b(block|let*|return-from|catch|load-time-value|setq|eval-when|locally|symbol-macrolet|flet|macrolet|tagbody|function|multiple-value-call|the|go|multiple-value-prog1|throw|if|progn|unwind-protect|labels|progv|let|quote|defclass|defconstant|defgeneric|defmacro|defmethod|defpackage|defparameter|defsetf|defstruct|deftype|defun|defvar|define-[^\\s]+)\\b"
        :keyword "&(whole|environment|optional|key|rest|body|allow-other-keys|aux)|:[\\w\\d-]+"))

(defun determine-regions (text &key (tokens *lisp-tokens*)
                                    (theme *monokai-theme*))
  (let ((regions ()))
    (flet ((parse (regex color)
             (cl-ppcre:do-matches (s e regex text)
               (push (list s e color) regions))))
      (parse (getf tokens :comment) (getf theme :comment))
      (parse (getf tokens :string) (getf theme :string))
      (parse (getf tokens :keyword) (getf theme :keyword))
      (parse (getf tokens :construct) (getf theme :construct)))
    (values (getf theme :text)
            (nreverse regions))))
