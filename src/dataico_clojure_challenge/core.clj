(ns dataico-clojure-challenge.core
  (:require
    [clojure.data.json :as json]
    [invoice-spec :as i]
    [clojure.spec.alpha :as s1]
    [invoice-item :as ii]
    [clojure.test :as t]))

;; Problem 1

(def invoice (clojure.edn/read-string (slurp "invoice.edn")))

(defn
  valid-items
  "Returns items that satisfies just one of the following:\n - At least have one item that has :iva 19%\n - At least one item has retention :ret_fuente 1%"
  [invoice]
  (->> invoice
       :invoice/items
       (filter (fn [item]
                 (let [
                       with-iva-19
                       (some #(and (= :iva (:tax/category %)) (= 19 (:tax/rate %)) %) (get-in item [:taxable/taxes])),
                       with-retention-1
                       (some #(and (= :ret_fuente (:retention/category %)) (= 1 (:retention/rate %))) (get-in item [:retentionable/retentions]))]
                   (and (or with-iva-19 with-retention-1) (not (and with-iva-19 with-retention-1))))))))

(println "[Problem 1] Items that satisfy just one of the conditions: " (valid-items invoice))

;; Problem 2

(def json-invoice (json/read-str (slurp "invoice.json")
                                :key-fn keyword))

(defn parse-date
  "Parses a date string in dd/MM/yyyy format to a valid Date"
  [date-string] (.parse
                  (java.text.SimpleDateFormat. "dd/MM/yyyy") date-string))

(defn rename-customer-keys
  [invoice]
  (update invoice :customer #(clojure.set/rename-keys
                                          %
                                          {:company_name :customer/name,
                                           :email :customer/email})))

(defn rename-item-taxes-keys
  [item]
  (update item :taxes
          (fn [taxes] (mapv #(clojure.set/rename-keys
                               (update
                                 (update % :tax_rate (fn [rate] (double rate)))
                                 :tax_category
                                 (fn [category] (if (= "IVA" category) :iva category)))
                               {:tax_category :tax/category, :tax_rate :tax/rate})
                            taxes))))
(rename-item-taxes-keys {:price 10000.0,
                         :quantity 1.0,
                         :sku "SUPER-1",
                         :taxes [{:tax_category "IVA", :tax_rate 5}]})

(defn rename-items-keys
  [invoice]
  (update invoice :items
          (fn [items] (mapv #(clojure.set/rename-keys
                              (rename-item-taxes-keys %)
                              {:price :invoice-item/price,
                               :quantity :invoice-item/quantity,
                               :sku :invoice-item/sku,
                               :taxes :invoice-item/taxes})
                           items))))

(defn rename-keys
  [invoice]
  "Change keys names from json data in order to pass spec validation (just the ones necessary)"
    (clojure.set/rename-keys
      (rename-items-keys (rename-customer-keys invoice))
      {:issue_date :invoice/issue-date,
       :customer :invoice/customer,
       :items :invoice/items}))

(defn prepare-invoice
  [json-invoice]
  "Change keys names from json data in order to pass spec validation"
  (update (rename-keys (:invoice json-invoice)) :invoice/issue-date parse-date)
  )

(memoize prepare-invoice)

println(str "[Problem 2] Valid?: " (s1/valid? ::i/invoice (prepare-invoice json-invoice)))
(s1/explain ::i/invoice (prepare-invoice json-invoice))


;; Problem 3

(t/deftest
  no-discount-rate
  (t/is (= 10000.0 (ii/subtotal {:invoice-item/precise-price 10000.0,
                                 :invoice-item/precise-quantity 1})))
  (t/is (= 40000.0 (ii/subtotal {:invoice-item/precise-price 20000.0,
                                 :invoice-item/precise-quantity 2})))
  (t/is (= 48000.0 (ii/subtotal {:invoice-item/precise-price 24000.0,
                                 :invoice-item/precise-quantity 2,
                                 :invoice-item/discount-rate 0})))
  )

(t/deftest
  no-price
  (t/is (= 0.0 (ii/subtotal {:invoice-item/precise-price 0.0,
                             :invoice-item/precise-quantity 1})))
  )

(t/deftest
  no-quantity
  (t/is (= 0.0 (ii/subtotal {:invoice-item/precise-price 10000.0,
                             :invoice-item/precise-quantity 0})))
  )

(t/deftest
  discount-rates
  (t/is (= 0.0 (ii/subtotal {:invoice-item/precise-price 0.0,
                             :invoice-item/precise-quantity 1,
                             :invoice-item/discount-rate 10})))
  (t/is (= 18000.0 (ii/subtotal {:invoice-item/precise-price 10000.0,
                                 :invoice-item/precise-quantity 2,
                                 :invoice-item/discount-rate 10})))
  (t/is (= 15000.0 (ii/subtotal {:invoice-item/precise-price 10000.0,
                                 :invoice-item/precise-quantity 2,
                                 :invoice-item/discount-rate 25})))
  (t/is (= 10000.0 (ii/subtotal {:invoice-item/precise-price 10000.0,
                                 :invoice-item/precise-quantity 2,
                                 :invoice-item/discount-rate 50})))
  )

(t/deftest
  max-discount
  (t/is (= 0.0 (ii/subtotal {:invoice-item/precise-price 10000.0,
                                 :invoice-item/precise-quantity 2,
                                 :invoice-item/discount-rate 100})))
  )

(t/deftest
  floating-point-discount
  (t/is (= 19980.0 (ii/subtotal {:invoice-item/precise-price 10000.0,
                             :invoice-item/precise-quantity 2,
                             :invoice-item/discount-rate 0.1})))
  )

(t/run-tests)