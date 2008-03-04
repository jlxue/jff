/**
 * 预处理混淆的 class 文件，去掉一些反反编译的技巧，
 * 便于 jad.exe 反编译。
 *
 * 代码依赖 www.apache.org 的 BCEL 字节码处理库。
 *
 * 用法：
 *  编译
 *  javac -classpath bcel-5.2.jar S.java
 *  
 *  运行
 *      比如将 bcel, S.class 放在父目录，当前处于 game 目录：
 *      set CLASSPATH=c:\WTK22\lib\cldcapi11.jar;c:\WTK22\lib\midpapi20.jar
 *      dir /s/b *.class | java -classpath %CLASSPATH%;..\bcel-5.2.jar;..;. S
 *
 *      在 UNIX 类系统上 classpath 用冒号分隔，执行完后相应的 class 文件被
 *      直接替换，所以请预先备份。
 *
 * 20070325 lyb 0.1 合并异常表
 * 20070403 lyb 0.9 重命名类成员
 *
 * BUG:
 *      效率很低，因为重复的查找数组，但耗时还在可用范围 :-)
 *      对于 try catch 内包含 return 的情况，jad 处理不了
 *      有的构造函数即使用 jad -f 也是不会带上包名，导致编译出错
 */
import java.io.*;
import java.util.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.util.SyntheticRepository;

public class S {
    public static void main(String[] args) {
        SyntheticRepository repos = SyntheticRepository.getInstance();
        ClassParser parser;
        JavaClass clazz;
        JavaClass[] clazzes;
        S s = new S();
        int i;

        if (args.length == 0)
            args = getClassList(System.in);
        if (args == null || args.length == 0)
            return;

        clazzes = new JavaClass[args.length];

        // 类内部的处理
        for (i = 0; i < args.length; ++i) {
            System.out.println("Processing class: " + args[i]);
            try {
                parser = new ClassParser(args[i]);
                clazz = parser.parse();
            } catch (Exception e) {
                e.printStackTrace();
                return;
            }

            Method[] methods = clazz.getMethods();
            for (int j = 0; j < methods.length; ++j) {
                s.mergeExceptionTable(methods[j]);
            }

            clazzes[i] = clazz;
            repos.storeClass(clazz);
        }

        // 涉及多个类之间的处理
        renameFieldsAndMethods(clazzes);

        // dump all classes
        for (i = 0; i < args.length; ++i) {
            try {
                if (null != clazzes[i])
                    clazzes[i].dump(args[i]);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public static String[] getClassList(InputStream ins) {
        String line;
        BufferedReader reader = null;
        ArrayList list = new ArrayList();
        try {
            reader = new BufferedReader(new InputStreamReader(ins));
            while (null != (line = reader.readLine()))
                list.add(line);
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                reader.close();
            } catch (IOException ee) {
                ee.printStackTrace();
            }
        }

        String[] s = new String[list.size()];
        for (int i = 0; i < s.length; ++i)
            s[i] = (String)list.get(i);
        return s;
    }

    public static void renameFieldsAndMethods(JavaClass[] clazzes) {
        ConstantPool[] pools;
        ArrayList[] newpools;       /* 每一个 newpool 对应一个 Constant[]，供动态添加,
                                     * 每更名一个 ConstantCP，则增加一个 ConstantUtf8
                                     * 和一个 ConstantNameAndIndexType，后者的 name 就是
                                     * 前者。
                                     */
        int[][] newFieldNames = new int[clazzes.length][];
        int[][] newMethodNames = new int[clazzes.length][];
        int i, j;

        System.out.println("renameFieldsAndMethods(): clazzes.length=" + clazzes.length);

        pools = new ConstantPool[clazzes.length];
        newpools = new ArrayList[clazzes.length];
        for (i = 0; i < clazzes.length; ++i) {
            if (null == clazzes[i])
                continue;

            pools[i] = clazzes[i].getConstantPool();
            newpools[i] = new ArrayList(64);
            Constant[] cp = pools[i].getConstantPool();
            for (j = 0; j < cp.length; ++j)
                newpools[i].add(cp[j]);

            newFieldNames[i] = new int[clazzes[i].getFields().length];
            newMethodNames[i] = new int[clazzes[i].getMethods().length];
            for (j = 0; j < newFieldNames[i].length; ++j)
                newFieldNames[i][j] = -1;
            for (j = 0; j < newMethodNames[i].length; ++j)
                newMethodNames[i][j] = -1;
        }

        // 重命名类的成员变量和方法，在名字后面添加 "_" + i + "_" + j
        for (i = 0; i < pools.length; ++i) {
            if (null == pools[i])
                continue;
            
            // 类的成员变量
            Field[] fields = clazzes[i].getFields();
            for (j = 0; j < fields.length; ++j) {
                // 混淆后的名字一般少于三个字符
                if (fields[j].getName().length() > 2)
                    continue;
                int[] ij = findFieldOrMethod(clazzes, i,
                        fields[j].getName(), fields[j].getSignature(), true);
                renameFieldOrMethod(j, fields[j].getName() + "_" + ij[0] + "_" + ij[1] + "_f",
                        newpools[i], newFieldNames[i]);
            }

            // 类的方法
            Method[] methods = clazzes[i].getMethods();
            for (j = 0; j < methods.length; ++j) {
                // 混淆后的名字一般少于三个字符
                if (methods[j].getName().length() > 2)
                    continue;
                int[] ij = findFieldOrMethod(clazzes, i,
                        methods[j].getName(), methods[j].getSignature(), false);
                renameFieldOrMethod(j, methods[j].getName() + "_" + ij[0] + "_" + ij[1] + "_m",
                        newpools[i], newMethodNames[i]);
            }

            // 常量池中的 field or method
            Constant[] cp = pools[i].getConstantPool();
            for (j = 0; j < cp.length; ++j) {
                if (! (cp[j] instanceof ConstantCP))
                    continue;

                ConstantCP cfm = (ConstantCP)cp[j];
                String classname = cfm.getClass(pools[i]);
                if (classname.startsWith("java.") || classname.startsWith("javax."))
                    continue;

                ConstantNameAndType nametype = (ConstantNameAndType)cp[cfm.getNameAndTypeIndex()];
                System.out.println("got: " + clazzes[i].getClassName() + "|"+nametype.getName(pools[i]) + " " + nametype.getSignature(pools[i]));
                // 混淆后的名字一般少于三个字符
                if (nametype.getName(pools[i]).length() > 2)
                    continue;

                System.out.println("----fieldOrMethod: " + cfm);
                System.out.println("----classname=" + classname + " clazzes[i].name=" + clazzes[i].getClassName());
                int n = i;
                if (! classname.equals(clazzes[i].getClassName()))
                    n = findClassByName(clazzes, classname);

                boolean isField = cfm instanceof ConstantFieldref;
                int[] ij = findFieldOrMethod(clazzes, n,
                        nametype.getName(pools[i]), nametype.getSignature(pools[i]),
                        isField);
                System.out.println("i=" + i + " n=" + n + " ij[]: " + ij[0] + " " + ij[1]);
                if (i == ij[0]) { // 使用 field or method 改名后相应的 name index
                    renameConstantFieldOrMethod(cfm, j, nametype,
                                null /* ignored */,
                            newpools[i], isField ? newFieldNames[i][ij[1]] : newMethodNames[i][ij[1]]);
                } else {
                    renameConstantFieldOrMethod(cfm, j, nametype,
                                nametype.getName(pools[i]) + "_" + ij[0] + "_" + ij[1] + (isField ? "_f" : "_m"),
                            newpools[i], -1 /* ignored */);
                }
            }
        }

        /* 设置新的 Constant[] 给 ConstantPool */
        for (i = 0; i < clazzes.length; ++i) {
            System.out.println(clazzes[i].getClassName());
            if (null == clazzes[i] || newpools[i].size() == pools[i].getLength())
                continue;
            System.out.println("+++" + clazzes[i].getClassName());
            
            Constant[] c = new Constant[newpools[i].size()];
            for (j = 0; j < c.length; ++j)
                c[j] = (Constant)newpools[i].get(j);
            pools[i].setConstantPool(c);

            Field[] fields = clazzes[i].getFields();
            for (j = 0; j < fields.length; ++j)
                if (newFieldNames[i][j] > 0) {
                    System.out.println("fields[j]=" + fields[j].getName());
                    fields[j].setNameIndex(newFieldNames[i][j]);
                    System.out.println("new fields[j]=" + fields[j].getName());
                }

            Method[] methods = clazzes[i].getMethods();
            for (j = 0; j < methods.length; ++j)
                if (newMethodNames[i][j] > 0) {
                    System.out.println("methods[j]=" + methods[j].getName());
                    methods[j].setNameIndex(newMethodNames[i][j]);
                    System.out.println("new methods[j]=" + methods[j].getName());
                }
        }
    }

    public static void renameFieldOrMethod(int j, String newname, ArrayList newpool, int[] newNames) {
        ConstantUtf8 utf = new ConstantUtf8(newname);
        newpool.add(utf);
        newNames[j] = newpool.size() - 1;
    }

    public static void renameConstantFieldOrMethod(ConstantCP cfm, int j,
            ConstantNameAndType old, String newname, ArrayList newpool, int k) {
        ConstantNameAndType newc = new ConstantNameAndType(old);
        ConstantUtf8 utf;
        if (k >= 0) {
            newc.setNameIndex(k);
        } else {
            utf = new ConstantUtf8(newname);
            newpool.add(utf);
            newc.setNameIndex(newpool.size() - 1);
        }

        newpool.add(newc);
        ConstantCP newcfm = (ConstantCP)cfm.copy();
        newcfm.setNameAndTypeIndex(newpool.size() - 1);
        newpool.set(j, newcfm);
    }

    public static int findClassByName(JavaClass[] clazzes, String classname) {
        for (int i = 0; i < clazzes.length; ++i)
            if (classname.equals(clazzes[i].getClassName()))
                return i;
        
        return -1;
    }

    /* 从 clazzes[n] 类开始，查找某个成员，父类优先 */
    public static int[] findFieldOrMethod(JavaClass[] clazzes, int n,
            String name, String signature, boolean isField) {
        int[] ij = new int[2];
        int i, j;
        JavaClass clazz = clazzes[n];

        JavaClass[] superClazzes;
        JavaClass[] interfaces;
        try {
            superClazzes = clazz.getSuperClasses();
            interfaces = clazz.getAllInterfaces();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            throw new RuntimeException(e.getMessage());
        }

        for (i = superClazzes.length - 1; i >= 0; --i) {
            ij[0] = findClassByName(clazzes, superClazzes[i].getClassName());
            if (ij[0] < 0)
                continue;
            ij[1] = findFieldOrMethod(clazzes[ij[0]], name, signature, isField);
            if (ij[1] >= 0)
                return ij;
        }

        for (i = interfaces.length - 1; i >= 0; --i) {
            ij[0] = findClassByName(clazzes, interfaces[i].getClassName());
            if (ij[0] < 0)
                continue;
            ij[1] = findFieldOrMethod(clazzes[ij[0]], name, signature, isField);
            if (ij[1] >= 0)
                return ij;
        }

        ij[0] = n;
        ij[1] = findFieldOrMethod(clazz, name, signature, isField);
        if (ij[1] >= 0)
            return ij;

        throw new RuntimeException("Can't find fieldOrMethod: " + clazz.getClassName() + "|" + name + ": " + signature);
    }

    /* 在 clazz 类中查找某个成员 */
    public static int findFieldOrMethod(JavaClass clazz, String name, String signature, boolean isField) {
        FieldOrMethod[] fm;
        if (isField)
            fm = clazz.getFields();
        else
            fm = clazz.getMethods();

        for (int j = 0; j < fm.length; ++j)
            if (name.equals(fm[j].getName()) && signature.equals(fm[j].getSignature()))
                return j;

        return -1;
    }

    /*
     * 将混淆器故意分开的两个或者多个一样的异常处理器合并，比如
     * from     to      target      type
     * 0        55      78          55
     * 56       77      78          55
     * 这两个就能合并为一条：
     * 0        77      78          55
     */ 

    public void mergeExceptionTable(Method m) {
        Comparator cmp = new CodeExceptionComparator();
        Code code = m.getCode();
        if (null == code)
            return;

        CodeException[] table = code.getExceptionTable();
        if (null == table || table.length < 2)
            return;

        Arrays.sort(table, cmp);

        int len = table.length;
        for (int i = 1; i < table.length; ++i) {
            for (int j = 0; j < i && j < len; ++j) {
                if (table[i].getStartPC() - 1 == table[j].getEndPC() &&
                        table[i].getHandlerPC() == table[j].getHandlerPC() &&
                        table[i].getCatchType() == table[j].getCatchType()) {
                    --len;
                    table[j].setEndPC(table[i].getEndPC());
                    break;
                }
            }
        }

        if (len == table.length)
            return;
        else {
            System.out.println("    merge exception table: " + m);
            System.out.println("    table length, old=" + table.length + " new=" + len);
        }

        CodeException[] newtable = new CodeException[len];
        System.arraycopy(table, 0, newtable, 0, len);
        code.setExceptionTable(newtable);
    }

    class CodeExceptionComparator implements Comparator {
        public CodeExceptionComparator() {
        }

        public int compare(Object o1, Object o2) {
            CodeException e1 = (CodeException)o1;
            CodeException e2 = (CodeException)o2;

            if (e1.getStartPC() < e2.getStartPC())
                return -1;
            else if (e1.getStartPC() > e2.getStartPC())
                return 1;
            else { // equal start pc
                if (e1.getEndPC() < e2.getEndPC())
                    return -1;
                else if (e1.getEndPC() > e2.getEndPC())
                    return 1;
                else  { // equal end pc
                    if (e1.getHandlerPC() < e2.getHandlerPC())
                        return -1;
                    else if (e1.getHandlerPC() > e2.getHandlerPC())
                        return 1;
                    else
                        return 0;
                }
            }
        }
    }
}

