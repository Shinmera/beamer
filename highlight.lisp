#|
 This file is a part of beamer
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.beamer)

(defvar *themes* (make-hash-table :test 'eq))
(defvar *languages* (make-hash-table :test 'eq))
(defvar *default-theme* :monokai)
(defvar *default-language* NIL)

(defmethod theme ((name symbol))
  (multiple-value-bind (v p) (gethash name *themes*)
    (if p v (error "No theme called ~s." name))))

(defmethod (setf theme) (vars (name symbol))
  (setf (gethash name *themes*) vars))

(defun remove-theme (name)
  (remhash name *themes*)
  name)

(defmacro define-theme (name &body parts)
  (let ((g (gensym)))
    `(progn
       (setf (theme ',name)
             (list ,@(loop for (k v) on parts by #'cddr
                           collect k collect `(let ((,g ,v))
                                                (etypecase ,g
                                                  (string (hex->vec (parse-integer ,g :radix 16)))
                                                  (number (hex->vec ,g))
                                                  (vec3 (vec4 (vx ,g) (vy ,g) (vz ,g) 1))
                                                  (vec4 ,g))))))
       ',name)))

(defun hex->vec (hex)
  (vec4 (/ (ldb (byte 8 16) hex) 255)
        (/ (ldb (byte 8 8) hex) 255)
        (/ (ldb (byte 8 0) hex) 255)
        1))

(defmethod language ((name symbol))
  (multiple-value-bind (v p) (gethash name *languages*)
    (if p v (error "No language called ~s." name))))

(defmethod (setf language) (vars (name symbol))
  (setf (gethash name *languages*) vars))

(defun remove-language (name)
  (remhash name *languages*)
  name)

(defmacro define-language (name &body parts)
  `(progn
     (setf (language ',name)
           (list ,@parts))
     ',name))

(define-theme NIL
  :text "f8f8f2")

(define-language NIL)

(define-theme :monokai
  :text "f8f8f2"
  :comment "75715e"
  :keyword "66d9ef"
  :construct "f92672"
  :string "e6db74")

(define-language :glsl
  :comment "//.*(\\n|$)"
  :string "\"[^\"]*\""
  :construct "\\b(while|using|typedef|this|template|switch|subroutine|struct|sizeof|return|restrict|out|output|notinline|namespace|layout|input|inout|inline|in|interface|invariant|if|goto|for|external|extern|enum|else|do|discard|default|continue|cont|class|cast|case|break|writeonly|volatile|union|uniform|resource|readonly|public|precision|patch|partition|attribute|asm|active|const|common|coherent)\\b|#[\\w\\d]+"
  :keyword "\\b(void|vec4|vec3|vec2|varying|uvec4|uvec3|uvec2|usamplerCubeArray|usamplerCube|usamplerBuffer|usampler3D|usampler2DRect|usampler2DMSArray|usampler2DMS|usampler2DArray|usampler2D|usampler1DArray|usampler1D|unsigned|uint|uimageCubeArray|uimageCube|uimageBuffer|uimage3D|uimage2DRect|uimage2DMSArray|uimage2DMS|uimage2DArray|uimage2D|uimage1DArray|uimage1D|true|superp|static|smooth|smapler2DRectShadow|short|shared|samplerCubeShadow|samplerCubeArrayShadow|samplerCubeArray|samplerCube|samplerBuffer|sampler3DRect|sampler3D|sampler2DShadow|sampler2DRect|sampler2DMSArray|sampler2DMS|sampler2DArrayShadow|sampler2DArray|sampler2D|sampler1DShadow|sampler1DArrayShadow|sampler1DArray|sampler1D|sample|precise|noperspective|mediump|mat4x4|mat4x3|mat4x2|mat4|mat3x4|mat3x3|mat3x2|mat3|mat2x4|mat2x3|mat2x2|mat2|lowp|long|ivec4|ivec3|ivec2|isamplerCubeArray|isamplerCube|isamplerBuffer|isampler3D|isampler2DRect|isampler2DMSArray|isampler2DMS|isampler2DArray|isampler2D|isampler1DArray|isampler1D|int|imageCubeArray|imageCube|imageBuffer|image3D|image2DRect|image2DMSArray|image2DMS|image2DArray|image2D|image1DArray|image1D|iimageCubeArray|iimageCube|iimageBuffer|iimage3D|iimage2DRect|iimage2DMSArray|iimage2DMS|iimage2DArray|iimage2D|iimage1DArray|iimage1D|hvec4|hvec3|hvec2|highp|half|fvec4|fvec3|fvec2|float|flat|fixed|filter|false|dvec4|dvec3|dvec2|double|dmat4x4|dmat4x3|dmat4x2|dmat4|dmat3x4|dmat3x3|dmat3x2|dmat3|dmat2x4|dmat2x3|dmat2x2|dmat2|centroid|bvec4|bvec3|bvec2|buffer|bool|atomic_uint)\\b")

(define-language :lisp
  :comment ";.*(\\n|$)"
  :string "\"[^\"]*\""
  :construct "\\b(block|let*|return-from|catch|load-time-value|setq|eval-when|locally|symbol-macrolet|flet|macrolet|tagbody|function|multiple-value-call|the|go|multiple-value-prog1|throw|if|progn|unwind-protect|labels|progv|let|quote|defclass|defconstant|defgeneric|defmacro|defmethod|defpackage|defparameter|defsetf|defstruct|deftype|defun|defvar|define-[^\\s]+)\\b"
  :keyword "&(whole|environment|optional|key|rest|body|allow-other-keys|aux)|(^|[^:\\w]):[\\w\\d-]+")

(defun determine-regions (text &key (language *default-language*) (theme *default-theme*))
  (let ((regions ())
        (language (language language))
        (theme (theme theme)))
    (flet ((parse (regex color)
             (cl-ppcre:do-matches (s e regex text)
               (push (list s e color) regions))))
      (loop for (key regex) on language by #'cddr
            for color = (getf theme key)
            do (when color (parse regex color))))
    (values (getf theme :text)
            (nreverse regions))))
