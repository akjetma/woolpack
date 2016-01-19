# woolpack

A wordcloud layout algorithm in Clojure.

## Basic Usage

```
(require '[woolpack.core :as cloud])

(def words 
     [{:word "vapor" :width 100 :height 20} 
      {:word "wave" :width 40 :height 10}])

(cloud/billow words)

#=> 
{:x-max 100 
 :y-max 30 
 :placed ({:word "vapor" :width 100 :height 20 :x 0 :y 0 :rotation 0} 
          {:word "wave" :width 40 :height 10 :x 100 :y 30 :rotation 180})}
```

## License

Copyright © 2016 Adam Jetmalani

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
