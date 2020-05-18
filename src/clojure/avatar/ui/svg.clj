; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 2.0 (http://www.eclipse.org/legal/epl-v20.html)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns avatar.ui.svg
  (:require [clj-jfx.core :as jfx]
            [avatar.util :as u]
            [avatar.ui.plot.layout :as lo]
            [clojure.string :as str]
            [avatar.ui.plot.painting :as paint]
            [avatar.ui.plot.util :as pu]
            [clj-jfx.colors :as col])
  (:import (org.w3c.dom Document Element Node)
           (org.apache.batik.transcoder.svg2svg SVGTranscoder)
           (org.apache.batik.transcoder TranscoderInput TranscoderOutput Transcoder)
           (java.io FileOutputStream OutputStreamWriter OutputStream ByteArrayOutputStream ByteArrayInputStream)
           (org.apache.fop.svg PDFTranscoder)
           (org.apache.batik.anim.dom SVGDOMImplementation)
           (org.apache.batik.transcoder.image PNGTranscoder TIFFTranscoder)))



(defn document
  []
  (.createDocument
    (SVGDOMImplementation/getDOMImplementation)
    SVGDOMImplementation/SVG_NAMESPACE_URI,
    "svg",
    nil))


(defn owner-document
  [^Element element]
  (.getOwnerDocument element))



(defn root-element
  {:inline (fn [doc]
             `(let [^Document doc# ~doc,
                    ^Element e# (.getDocumentElement doc#)]
                e#))}
  ^Element [^Document doc]
  (.getDocumentElement doc))


(defn attribute!
  [^Element element, attribute-name, attribute-value]
  (doto element
    (.setAttribute (name attribute-name), (str attribute-value))))


(defn attribute
  [^Element element, attribute-name]
  (let [val-str (.getAttribute element (name attribute-name))]
    (when-not (str/blank? val-str)
      val-str)))


(defn long-attribute
  ^long [^Element element, attribute-name]
  (let [val-str (.getAttribute element (name attribute-name))]
    (when-not (str/blank? val-str)
      (Long/parseLong val-str))))


(defn double-attribute
  ^double [^Element element, attribute-name]
  (let [val-str (.getAttribute element (name attribute-name))]
    (when-not (str/blank? val-str)
      (Double/parseDouble val-str))))


(defn document-size!
  [doc, ^double width, ^double height]
  (doto doc
    (->
      (root-element)
      (attribute! :width width)
      (attribute! :height, height))))


(defn append!
  [doc-or-element, ^Node child-element]
  (when child-element
    (let [^Node element (cond-> doc-or-element (instance? Document doc-or-element) root-element)]
      (.appendChild element child-element)))
  doc-or-element)


(defn append-elements!
  [doc-or-element, element-list]
  (let [^Node parent-element (cond-> doc-or-element (instance? Document doc-or-element) root-element)]
    (reduce
      (fn [^Node parent-element, child-element]
        (doto parent-element
          (.appendChild child-element)))
      parent-element
      element-list)
    doc-or-element))


(defn import-element
  [^Document doc, ^Element element]
  (.importNode doc, element, true))


(defn create-element
  ^Element [^Document doc, element-class]
  (.createElementNS doc, SVGDOMImplementation/SVG_NAMESPACE_URI, (name element-class)))


(defn group
  [doc]
  (create-element doc "g"))


(defn size!
  [^Element element, width, height]
  (-> element
    (attribute! :width, (str width))
    (attribute! :height, (str height))))

(defn rectangle
  [^Document doc, width, height]
  (-> (create-element doc, "rect")
    (size! (inc width), (inc height))))


(defn attribute-ns!
  ([^Element element, attribute-name, attribute-value]
   (attribute-ns! element, SVGDOMImplementation/SVG_NAMESPACE_URI, attribute-name, attribute-value))
  ([^Element element, ns, attribute-name, attribute-value]
   (doto element
     (.setAttributeNS ns, (name attribute-name), (str attribute-value)))))


(defn position!
  [^Element element, x, y]
  (-> element
    (attribute! :x, (str x))
    (attribute! :y, (str y))))


(defn rotate!
  [^Element element, angle, x, y]
  (-> element
    (attribute! "transform"
      (format "rotate(%d,%f,%f)" angle, (double x), (double y)))))


(defn id!
  [^Element element, id]
  (attribute! element, :id, (str id)))


(defn rectangle
  [doc, width, height]
  (-> (create-element doc, "rect")
    (size! width, height)))


(defn line
  [^Document doc, x1, y1, x2, y2]
  (-> (create-element doc, "line")
    (attribute! :x1 x1)
    (attribute! :y1 y1)
    (attribute! :x2 x2)
    (attribute! :y2 y2)))


(defn fill-color!
  [^Element element, color]
  (-> element
    (attribute! :fill, (col/color->str color))))


(defn stroke!
  [^Element element, color, width]
  (-> element
    (attribute! :stroke, (or (some-> color col/color->str) "black"))
    (attribute! :stroke-width, (str (or width 1)))))


(defn from-set
  [set, value]
  (when value
    (if (contains? set value)
      (name value)
      (u/illegal-argument "Value must be one of the following: %s" (str/join ", " set)))))


(defn text-content!
  [^Element element, ^String text]
  (doto element
    (.setTextContent text)))


(defn text
  [^Document doc, some-text & {:keys [anchor, dominant-baseline]}]
  (let [anchor (from-set #{:start, :middle, :end} anchor)
        dominant-baseline (from-set #{:auto :use-script :no-change :reset-size :ideographic :alphabetic :hanging :mathematical :central :middle :text-after-edge :text-before-edge}
                            dominant-baseline)]
    (doto (create-element doc, "text")
      (cond-> some-text (text-content! some-text))
      (cond-> anchor (attribute! :text-anchor anchor))
      (cond-> dominant-baseline (attribute! :dominant-baseline dominant-baseline)))))


(defn text-span
  [^Document doc, text]
  (-> (create-element doc, "tspan")
    (text-content! text)))


(defn font!
  [^Element element {:keys [family, weight, posture, size]}]
  (-> element
    (cond-> family (attribute! :font-family family))
    (cond-> size (attribute! :font-size size))
    (cond-> weight (attribute! :font-weight (.getWeight (jfx/font-weight weight))))
    (cond-> posture (attribute! :font-style (get {:regular "normal", :italic "italic"} posture)))))


#_(defn transcode
    "manual svg to string implementation with lock unicode replacement"
  [^TranscoderInput input, ^TranscoderOutput output]
  (if-let [doc (.getDocument input)]
    (let [tf (TransformerFactory/newInstance)
          transformer (.newTransformer tf)
          writer (.getWriter output)
          sw (StringWriter. 1024),
          _ (.transform transformer (DOMSource. doc), (StreamResult. sw))
          svg-str (.toString sw)]
      (.write writer (str/replace svg-str "&#55357;&#56594;" "&#128274;")))
    (throw (Error. "Reader or Document expected"))))


(defn transcode-svg
  [^Document doc, ^OutputStream ostream]
  (let [owriter (OutputStreamWriter. ostream, "UTF-8")]
    (.transcode (SVGTranscoder.)
      (TranscoderInput. doc)
      (TranscoderOutput. owriter))
    (.flush owriter)
    (.close owriter)))


(defn save-svg
  [^String filename, ^Document doc]
  (transcode-svg doc, (FileOutputStream. filename)))


(defn save-as
  [^String filename, ^Document doc, ^Transcoder transcoder]
  (let [bout (ByteArrayOutputStream.)]
    (transcode-svg doc, bout)
    (.transcode transcoder
      (TranscoderInput. (ByteArrayInputStream. (.toByteArray bout)))
      (TranscoderOutput. (FileOutputStream. filename)))))


(defn save-pdf
  [^String filename, ^Document doc]
  (save-as filename, doc, (PDFTranscoder.)))


(defn save-png
  [^String filename, ^Document doc]
  (save-as filename, doc, (PNGTranscoder.)))


(defn save-tiff
  [^String filename, ^Document doc]
  (save-as filename, doc, (TIFFTranscoder.)))


(defn vertical-centered-text-coordinate
  ^double [^double y ^double height, ^double text-height]
  (+ y (/ height 2.0) (/ text-height 3.0)))


(defn centered-text-in-rect
  [doc, x, y, width, height, anchor, font, horizontal-margin, display-text]
  (let [text-height (lo/text-height font, display-text)]
    (-> (text doc, display-text, :anchor anchor)
      (position!
        (case anchor
          :start (+ x horizontal-margin)
          :middle (+ x (quot width 2))
          :end (- (+ x width) horizontal-margin)),
        (vertical-centered-text-coordinate y, height, text-height))
      (font! font))))


(defn centered-vertical-text-in-rect
  [doc, x, y, width, height, anchor, font, horizontal-margin, display-text]
  (let [text-height (lo/text-height font, display-text)
        x' (+ x (/ (- width height) 2.0))
        y' (+ y height)]
    (-> (text doc, display-text, :anchor anchor)
      (position!
        (case anchor
          :start (+ x' horizontal-margin)
          :middle (+ x' (quot width 2))
          :end (- (+ x' width) horizontal-margin)),
        (vertical-centered-text-coordinate y', height, text-height))
      (rotate! -90, x', y')
      (font! font))))


(defn centered-text-group-in-rect
  [doc, x, y, width, height, anchor, horizontal-margin, spacing, text+font-pair-list]
  (when (seq text+font-pair-list)
    (let [text-height (u/reduce-max (fn [{:keys [text, font]}] (lo/text-height font, text)) text+font-pair-list),
          n-1 (dec (count text+font-pair-list)),
          total-text-width (->> text+font-pair-list
                             (mapv (fn [{:keys [text, font]}] (lo/text-width font, text)))
                             (reduce + 0))
          x_t (case anchor
                :start (+ x horizontal-margin)
                :middle (u/illegal-argument ":middle not supported!")
                :end (- (+ x width) horizontal-margin))
          y_t (vertical-centered-text-coordinate y, height, text-height),
          text-node (-> (text doc, nil, :anchor anchor)
                      (position! x_t, y_t))]
      (doseq [{:keys [text, font]} text+font-pair-list]
        (append! text-node
          (-> (text-span doc, text)
            (font! font))))
      text-node)))




