set -e

mkdir -p ex1
cp ex_lisp.scala ex1/lisp.scala
cp build.sbt ex1/build.sbt
cp utils.scala ex1/utils.scala
cp DO.md ex1/README.md
zip -r ex1.zip ex1
