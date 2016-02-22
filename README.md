# StructuredQueryBuilder

## setup
Put the code below into `build.sbt`.

```
libraryDependencies +=  "com.noriakihoriuchi" %% "structured-query-builder" % "0.0.2"

resolvers += "com.noriakihoriuchi" at "http://NoriakiHoriuchi.github.io/StructuredQueryBuilder"
```

## usage

Import classes below.

```
import StructuredQueryBuilder._
import StructuredQueryBuilder.Implicits._
```

If you don't set any option, set Nil on the first argument.

```
and(Nil, Seq("keyString" -> "valueString")).build // (and keyString:'valueString')
```

Please see more sample in the [test code](https://github.com/NoriakiHoriuchi/StructuredQueryBuilder/blob/master/src/test/scala/com/noriakihoriuchi/sqb/StructuredQueryBuilderSpec.scala).
