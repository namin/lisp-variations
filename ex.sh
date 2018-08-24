set -e

mkdir -p ex
cp ex_lisp.scala ex/lisp.scala
cp build.ex ex/build.sbt
cp utils.scala ex/utils.scala
cp DO.md ex/README.md
mv ex ex1
zip -r ex1.zip ex1


