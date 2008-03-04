文件用途
--------
Hello   EcpliseME 的模板工程
S.java  预处理 Java 类，去掉混淆技巧，便于 jad 顺利反编译
S.pl    调用 jad，native2ascii，生成 Eclipse 工程等
bcel-5.2.jar    字节码处理库，apache.org 出品
jad.exe         最好的 Java 反编译工具，可惜没有源码
Sword&Rose_X859.jar/jad     圣剑玫瑰原始应用
SwordRose_X859-ecplise.rar  反编译后的圣剑玫瑰 Eclipse 工程

使用示例
--------
解压缩 Sword&Rose_X859.jar 到 Sword&Rose_X859;
javac -classpath bcel-5.2.jar S.java
cd Sword&Rose_X859
dir /s/b *.class | java -classpath c:\WTK22\lib\cldcapi11.jar;c:\WTK22\lib\midpapi20.jar;..\bcel-5.2.jar;.. S 
cd ..
perl S.pl "Sword&Rose_X859" output

output 就是一个 Eclipse 工程，需要修改其中的 .eclipseme
和 .project 将 Sword&Rose_X859 中的“&”去掉。


后续操作
--------
圣剑玫瑰需要手工修正的地方：
1. 某些调用构造函数的地方没有写全包名，由于变量声明时给出了全类名，
所以很容易有修正；
2. try catch 内包含 return 时 jad 无法正确处理，解决办法是用 
javap -c ClassName 反编译出来对照里头的异常表更正 try catch，很容易。
3. e.class 里头一个语句混杂了 getstatic/pop 序列，导致前一个 getstatic
是无意义的，jad 识别不出来这种情况，对照 javap -c ClassName 的结果，
可以恢复出来。

