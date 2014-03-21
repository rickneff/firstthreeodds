#!/bin/bash
###############################################################################
#- File: savetyping.sh
#- Purpose: set up a CS 246 java project development environment
###############################################################################

#- declare and export shared environment variables
export EDITOR=emacs
export PH=${PROJECTHOME:=$PWD}
export PRJCT=${PROJECT:=$(basename $PH)}
export PRKEY=${PROJECTKEY:=$(echo $PRJCT | tr "[:upper:]" "[:lower:]")}
export BUILDDIR=$PH/build
export PPS=org.${PRKEY}.system
export SHARED=$HOME
export SHAREDBIN=$SHARED/bin
export SHAREDLIB=$SHARED/lib
export PATH=$SHAREDBIN:$PATH
export CP=$BUILDDIR/class:$BUILDDIR/test:$PH:$PH/src:$PH/test:$PH/lib/\*:$SHAREDLIB/\*
export CLASSPATH=$CP
export SLF4JRELEASE=slf4j-1.7.6
export SLF4JAPIJAR1=slf4j-api-1.7.6.jar
export SLF4JAPIJAR2=$(echo $SLF4JAPIJAR1 | sed -e 's/api/simple/')

# show help
h()
{
   echo -n export > tmpA$$ && echo ' -f' >> tmpA$$
   echo -n \#> tmpB$$ && echo ' ' >> tmpB$$
   grep -f tmpB$$ $PH/sdk/savetyping.sh | cut -d" " -f2- > tmpC$$
   grep -f tmpA$$ $PH/sdk/savetyping.sh | cut -d" " -f3- | paste - tmpC$$
   rm tmpA$$ tmpB$$ tmpC$$
}
export -f h
# make all build directories (if necessary)
mbd()
{
   mkdir -p $BUILDDIR/class
   mkdir -p $BUILDDIR/jar
   mkdir -p $BUILDDIR/jdoc/resources
}
export -f mbd
mbd
# connect to the project home directory
ph()
{
   cd $PH
}
export -f ph
# connect to a module home directory given the module name (outer.inner)
mh()
{
   outer=$(echo $1 | cut -d . -f2)
   inner=$(echo $1 | cut -d . -f3)
   cd $PH/src/org/$outer/$inner
}
export -f mh
# connect to the system module home directory
sy()
{
   mh $PPS
}
export -f sy
# build java source files in a module
b()
{
   
   if [ ! -f $PH/lib/$SLF4JAPIJAR1 -o ! -f $PH/lib/$SLF4JAPIJAR2 ]
   then
      curl -O http://www.slf4j.org/dist/$SLF4JRELEASE.zip
      jar xf $SLF4JRELEASE.zip $SLF4JRELEASE/$SLF4JAPIJAR1
      jar xf $SLF4JRELEASE.zip $SLF4JRELEASE/$SLF4JAPIJAR2
      mv $SLF4JRELEASE/*.jar $PH/lib
      rm -rf ${SLF4JRELEASE}*
   fi
   BASE=$(basename $PWD)
   if [ "$BASE" = "$PRJCT" ]
   then
      b1
   else
      JAR=$BUILDDIR/jar/${PRKEY}${BASE}.jar
      if [ -f $JAR ]; then
	 FILES=$(find . -name "*.java" -newer $JAR | tr "\n" " ")
      else
	 FILES=$(find . -name "*.java" | tr "\n" " ")
      fi
      if [ -z "$FILES" ]; then
	 echo "Nothing needs building in $BASE"
      else
	 echo "Building $FILES"
	 javac $* -d $BUILDDIR/class $FILES &&\
	    jar cf $JAR -C $BUILDDIR/class org/$PRKEY/$BASE &&\
	    echo "$JAR built."
      fi
   fi
}
export -f b
# connect to the system module home directory and build it
syb()
{
   sy
   b
}
export -f syb
# build the system module and then connect to the project home directory
b1()
{
   syb && ph
}
export -f b1
# build the system module, connect to the project home directory and make the project jar
b2()
{
   b1 && mj
}
export -f b2
# wipe (clean up, or remove) all build artifacts
wipe()
{
   rm -rf $BUILDDIR
   mbd
}
export -f wipe
# make jar
mj()
{
   MF=$1
   if [ -z "$MF" ]
   then
     MF=$PH/sdk/manifest.mf
   fi
   pushd $BUILDDIR/class > /dev/null
   for lib in $PH/lib/*.jar
   do
      if [ "$lib" = "$TESTINGLIB" ]
      then
         echo Skipping $(basename $lib)
      else
         echo Incorporating $(basename $lib) into $PRKEY.jar
         jar xf $lib
      fi
   done
   rm -rf META-INF
   popd > /dev/null
   if [ -f "$MF" ]
   then
      jar cfm $PRKEY.jar $MF -C $BUILDDIR/class .
   fi
}
export -f mj
# generate javadocs
jdoc()
{
   javadoc -quiet -author -link http://docs.oracle.com/javase/7/docs/api/\
       -d $PH/build/jdoc -sourcepath $PH/src -classpath $CP \
       -extdirs $PH/lib/ext -private -subpackages $PPS
}
export -f jdoc
###############################################################################
#- End of savetyping.sh project setup file
###############################################################################
