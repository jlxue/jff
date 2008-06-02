/**
 * Purpose: find dependent classes in class path for specified
 *          Java classes and dump them into a directory.
 *
 * Usage:
 *
 *   compile:
 *      javac -classpath bcel-5.2.jar JavaDep.java
 *
 *   run:
 *      see help information in showUsage().
 *
 * License:
 *   GPL v3
 *
 * Author:
 *   Liu Yubao <yubao.liu@gmail.com>
 *
 * ChangeLog:
 *   2008-06-03     Liu Yubao
 *      * initial release.
 */
import java.io.File;
import java.io.IOException;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;

import org.apache.bcel.Repository;
import org.apache.bcel.classfile.*;


public class JavaDep {
    Set depends = new HashSet();

    Set includes = new HashSet();
    Set excludes = new HashSet();
    String outputDir;


    public static void main(String[] args) {
        JavaDep javaDep = new JavaDep();
        Set classnames = new HashSet();

        for (int i = 0; i < args.length; ++i) {
            try {
                if (args[i].equals("-d")) {
                    javaDep.outputDir = args[++i];
                } else if (args[i].equals("-i")) {
                    javaDep.includes.add(args[++i]);
                } else if (args[i].equals("-x")) {
                    javaDep.excludes.add(args[++i]);
                } else {
                    classnames.add(args[i]);
                }
            } catch (ArrayIndexOutOfBoundsException e) {
                showUsage();
                return;
            }
        }

        if (classnames.size() == 0) {
            showUsage();
            return;
        }

        Iterator classname = classnames.iterator();
        while (classname.hasNext()) {
            javaDep.lookupDependency((String)classname.next());
        }

        javaDep.dump();
    }


    public void dump() {
        if (null != outputDir) {
            File dir = new File(outputDir);

            if (! dir.isDirectory() && ! dir.mkdirs()) {
                System.err.println("ERROR: can't create directory \"" + outputDir + "\"");
                outputDir = null;
            }
        }


        Iterator dep = depends.iterator();
        String name;
        while (dep.hasNext()) {
            name = (String)dep.next();
            if (excluded(name))
                continue;

            System.out.println(name);

            if (null != outputDir) {
                JavaClass clazz;
                try {
                    clazz = Repository.lookupClass(name);
                } catch (ClassNotFoundException e) {
                    System.err.println("Can't find " + name);
                    continue;
                }

                name = name.replace('.', '/');
                try {
                    // System.err.println("dumping " + name + ".class");
                    clazz.dump(outputDir + '/' + name + ".class");
                } catch (IOException e) {
                    System.err.println("Can't dump " + outputDir + '/' + name + ".class");
                }
            }
        }
    }


    public void lookupDependency(String classname) {
        // don't filter classname with excluded(classname), because we
        // must check the indirect dependency.
        if (classname.startsWith("java") || classname.startsWith("[") ||
                !depends.add(classname))
            return;

        JavaClass clazz;
        try {
            clazz = Repository.lookupClass(classname);
        } catch (ClassNotFoundException e) {
            System.err.println("Can't find " + classname);
            return;
        }

        ConstantPool cp = clazz.getConstantPool();
        Constant[] constants = cp.getConstantPool();

        for (int i = 0; i < constants.length; ++i) {
            if (constants[i] instanceof ConstantClass) {
                lookupDependency(cp.constantToString(constants[i]));
            }
        }
    }


    public boolean excluded(String classname) {
        Iterator exclude = excludes.iterator();
        while (exclude.hasNext()) {
            if (classname.startsWith((String)exclude.next()))
                return true;
        }

        if (includes.size() == 0)
            return false;

        Iterator include = includes.iterator();
        while (include.hasNext()) {
            if (classname.startsWith((String)include.next()))
                return false;
        }

        return true;
    }


    public static void showUsage() {
        System.err.println("Usage: java -classpath bcel-5.2.jar;. [options] JavaDep <class>...\n");
        System.err.println("  Options:");
        System.err.println("    -d  <outputDir>     dump classes into outputDir");
        System.err.println("    -i  <prefix>        only output classes whose names start with prefix");
        System.err.println("    -x  <prefix>        don't output classes whose names start with prefix");
        System.err.println("  -i and -x can occur many times.\n");
        System.err.println("  Example:");
        System.err.println("    java -classpath bcel-5.2.jar;. JavaDep -d bcel-mini -i org.apache.bcel JavaDep\n");
    }
}

