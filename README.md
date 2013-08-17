# uClassify Scala [![Build Status](https://travis-ci.org/ashwanthkumar/uClassify-scala.png)](https://travis-ci.org/ashwanthkumar/uClassify-scala/builds/)

Scala client for [uClassify Service API](http://www.uclassify.com/XmlApiDocumentation.aspx). This repo is WIP. 

## Client Usage
```
val client = UClassifyClient("__READ__API__KEY", "__WRITE__API__KEY")
client.createClassifier(classifierName)

val sampleTexts = List("she love me", "he hates me")
for {
  result <- client.classify(classifierName, sampleTexts)
} yield result.foreach(println)

```


## Example Usage
Refer Sample.scala under examples

## License
    Copyright 2013 Ashwanth Kumar
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
    
     http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

