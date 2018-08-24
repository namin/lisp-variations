set -e

mkdir -p ex1
cp ex_lisp.scala ex1/lisp.scala
cp build.ex ex1/build.sbt
cp utils.scala ex1/utils.scala
cp DO.md ex1/README.md
zip -r ex1.zip ex1

mkdir -p ex2
mkdir -p ex2/out
cp ex2_lisp.scala ex2/lisp0.scala
cd ex2
perl -pi -e 's/ex2_lisp/lisp0/g' lisp0.scala
cd ..
cp out/factorial.check.c ex2/out/
cp out/factorial.check.scala ex2/out/
cp out/odd.check.c ex2/out/
cp out/odd.check.scala ex2/out/
cp build.sbt ex2/build.sbt
cp lms.scala ex2/lms.scala
cp utils.scala ex2/utils.scala
cp DO2.md ex2/README.md
zip -r ex2.zip ex2
