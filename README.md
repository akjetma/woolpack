# woolpack

A wordcloud layout algorithm in Clojure.

```clojure
[akjetma/woolpack "0.1.5"]
```

## Basic Usage

```clojure
#=> (require '[woolpack.core :as cloud])
```

Input is a list of maps with `:width` and `:height` properties.

```clojure
#=> (def words 
      [{:word "vapor" :width 100 :height 20} 
       {:word "wave" :width 40 :height 10}])
```

Output is the list with `:x`, `:y`, and `:rotation` properties to each map.

```
(cloud/billow words)
#=> 
{:x-max 100 
 :y-max 30 
 :placed [{:word "vapor" :width 100 :height 20 :x 0 :y 0 :rotation 0} 
          {:word "wave" :width 40 :height 10 :x 100 :y 30 :rotation 180}]}
```
