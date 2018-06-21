(import
  os
  [hy [importer]])

(def lookup
  (let [[dir (os.path.dirname (os.path.abspath __file__))]]
    (dict-comp
      (-> f (.replace "make_" "") (.replace ".hy" ""))
      ;(import_module (+ "generators." f ".make_sample_set") :fromlist [(+ "generators." f)])
      (importer.import_file_to_module "generators" (os.path.join dir f))
      [f (list-comp
           f
           [f (os.listdir dir)]
           (and
             (f.endswith ".hy")
             (not (= f "__init__.hy"))))])))

(defn get-generator-name [g]
  (-> g.--file--
      (os.path.basename)
      (.replace ".hy" "")
      (.replace "make_" "")))

