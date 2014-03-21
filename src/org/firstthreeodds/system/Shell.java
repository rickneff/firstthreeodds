package org.firstthreeodds.system;

import java.io.*;

import java.lang.reflect.*;

import java.util.*;
import java.util.jar.*;
import java.util.zip.*;

/**
 * A class to encapsulate a command shell.
 *
 * @author Curtis Smith
 */
public class Shell
{
   /**
    * Array of error messages.
    */
   private static final String[] cErrors =
      {
         "The file or directory cannot be moved: ",
         "The file or directory cannot be removed: ",
         "The directory path cannot be created: ",
      };

   /**
    * An index of error message if the file or direectory
    * cannot be moved.
    */
   private static final int cError_Move = 0;

   /**
    * An index of error message if the file or directory
    * cannot be removed.
    */
   private static final int cError_Remove = 1;

   /**
    * An index of error message if the directory path
    * cannot be created.
    */
   private static final int cError_MakeDirectories = 2;

   /**
    * Holds output stream object
    */
   private OutputStream mOutputStream;

   /**
    * Holds error output stream object
    */
   private OutputStream mErrorStream;

   /**
    * Holds input stream object
    */
   private InputStream mStdin;

   /**
    * Holds print stream like System.out.
    */
   private PrintStream mStdout;

   /**
    * Holds print stream like System.error.
    */
   private PrintStream mStderr;

   /**
    * Holds whether verbose mode or not in shell
    */
   private boolean mVerbose;

   /**
    * Holds whether slient mode or not in shell
    */
   private boolean mSilent;

   /**
    * The map of string name to objects for a context
    */
   private Map<String, Object> mContext;

   /**
    * Creates a new Shell object with no parameter.
    * Silent mode is true.
    */
   public Shell()
   {
      this(true);
   }

   /**
    * Creates a new Shell object with one parameter.
    * Verbose mode is false.
    *
    * @param pSilent if silent mode is true(on) or false(off)
    */
   public Shell(boolean pSilent)
   {
      this(pSilent, false);
   }

   /**
    * Creates a new Shell object with two parameters
    * and initializes all member variables.
    *
    * @param pSilent True(on) or false(off) for silent mode
    * @param pVerbose True(on) or false(off) for verbose mode
    */
   public Shell(boolean pSilent, boolean pVerbose)
   {
      mOutputStream = System.out;
      mErrorStream = System.err;
      mStdout = System.out;
      mStderr = System.err;
      mSilent = pSilent;
      mVerbose = pVerbose;
      mContext = new HashMap<String, Object>();
   }

   /**
    * Gets if verbose mode is or not
    *
    * @return True of false for verbose mode 
    */
   public boolean getVerbose()
   {
      return mVerbose;
   }

   /**
    * Sets if verbose mode is or not
    *
    * @param pVerbose True or false for verbose mode
    *   
    * @return verbose mode before setting the parameter to the member varible
    */
   public boolean setVerbose(boolean pVerbose)
   {
      boolean verbose = mVerbose;
      mVerbose = pVerbose;

      return verbose;
   }

   /**
    * Gets if slient mode is or not
    *
    * @return true or false for slience mode
    */
   public boolean getSilent()
   {
      return mSilent;
   }

   /**
    * Sets if silent mode is or not
    *
    * @param pSilent boolean for silent mode
    *
    * @return silent mode before setting the parameter to the member varible 
    */
   public boolean setSilent(boolean pSilent)
   {
      boolean silent = mSilent;
      mSilent = pSilent;

      return silent;
   }

   /**
    * Gets a context of command
    *
    * @return command context
    */
   public Map<String, Object> getContext()
   {
      return mContext;
   }

   /**
    * Sets a context of command
    *
    * @param pContext Command context
    *
    * @return Command context before setting
    */
   public Map<String, Object> setContext(Map<String, Object> pContext)
   {
      Map<String, Object> context = mContext;
      mContext = pContext;

      return context;
   }

   /**
    * Gets an output stream
    *
    * @return An output stream
    */
   public OutputStream getOutputStream()
   {
      return mOutputStream;
   }

   /**
    * Sets an output steam object
    *
    * @param pOutputStream An output stream  
    *
    * @return An output stream before setting
    */
   public OutputStream setOutputStream(OutputStream pOutputStream)
   {
      OutputStream stream = mOutputStream;
      mOutputStream = pOutputStream;
      mStdout = new PrintStream(mOutputStream);

      return stream;
   }

   /**
    * Gets an error output stream
    *
    * @return An error output stream 
    */
   public OutputStream getErrorStream()
   {
      return mErrorStream;
   }

   /**
    * Sets an error output stream
    *
    * @param pErrorStream An error output stream
    *
    * @return an error output stream before setting
    */
   public OutputStream setErrorStream(OutputStream pErrorStream)
   {
      OutputStream stream = mErrorStream;
      mErrorStream = pErrorStream;
      mStderr = new PrintStream(mErrorStream);

      return stream;
   }

   /**
    * Gets an input stream
    *
    * @return An input stream
    */
   public InputStream getInputStream()
   {
      return mStdin;
   }

   /**
    * Sets an input stream
    *
    * @param pInputStream An input stream
    *
    * @return An input stream before setting
    */
   public InputStream setInputStream(InputStream pInputStream)
   {
      InputStream stream = mStdin;
      mStdin = pInputStream;

      return stream;
   }

   /**
    * Sets an input to input stream
    *
    * @param pString An input string
    *
    * @return An input string
    */
   public String setInput(String pString)
   {
      setInputStream(new ByteArrayInputStream(pString.getBytes()));

      return pString;
   }

   /**
    * Parses a command line
    *
    * @param pCommand A command string
    *
    * @return String array which is a parsed command
    */
   public String[] parse(String pCommand)
   {
      // Check for an empty command
      if (pCommand == null)
      {
         return null;
      }

      pCommand = pCommand.trim();

      if (pCommand.equals(""))
      {
         return new String[0];
      }

      // Prepare the command for the tokenizer
      StringBuffer buffer = new StringBuffer(pCommand);
      boolean quoted = false;

      if (pCommand.startsWith("echo "))
      {
         buffer.setCharAt(4, '\0');
      }
      else
      {
         for (int i = 0; i < buffer.length(); ++i)
         {
            char c = buffer.charAt(i);

            if (c == '\\')
            {
               if (i < (buffer.length() - 1))
               {
                  c = buffer.charAt(i + 1);

                  if ((c == '\\') || (c == '"'))
                  {
                     buffer.deleteCharAt(i);
                  }
               }
            }
            else if (c == '"')
            {
               buffer.deleteCharAt(i--);
               quoted = ! quoted;
            }
            else if (! quoted && Character.isWhitespace(c))
            {
               buffer.setCharAt(i, '\0');
            }
         }
      }

      // Tokenize to a string array
      List<String> list = new ArrayList<String>();

      StringTokenizer tokenizer = new StringTokenizer(new String(buffer), "\0");

      while (tokenizer.hasMoreTokens())
      {
         list.add(tokenizer.nextToken());
      }

      return (String[]) list.toArray(new String[0]);
   }

   /**
    * Prints out a message if verbose is true.
    *
    * @param pMessage A message string
    */
   public void println(String pMessage)
   {
      if (mVerbose)
      {
         mStdout.println(pMessage);
      }
   }

   /**
    * Print out a message
    *  
    * @param pMessage A message string
    */
   public void echo(String pMessage) // Echo cannot be made silent
   {
      mStdout.println(pMessage);
   }

   /**
    * Extracts a jar file
    *
    * @param pJar The jar file which wants to be extracted
    * @param pDirectory Directory where a jar file will be extracted
    *
    * @throws IOException Throws if a jar file cannot be extracted
    */
   public void extract(JarFile pJar, File pDirectory)
      throws IOException
   {
      byte[] bytes = new byte[0x80000];
      Enumeration enumeration = pJar.entries();

      while (enumeration.hasMoreElements())
      {
         ZipEntry entry = (ZipEntry) enumeration.nextElement();
         String name = entry.getName();
         File file = new File(pDirectory.getPath() + "/" + name);

         if (entry.isDirectory())
         {
            mkdir(file);
         }
         else
         {
            remove(file);

            File path = file.getParentFile();

            if (path != null)
            {
               mkdir(path);
            }

            InputStream source = pJar.getInputStream(entry);
            RandomAccessFile target = new RandomAccessFile(file, "rw");

            int i;

            while ((i = source.read(bytes)) != -1)
            {
               target.write(bytes, 0, i);
            }

            target.close();
            source.close();

            println("extracted: " + name + " --> " + file.getPath());
         }
      }
   }

   /**
    * Copies a file
    *
    * @param pSource The source file of copying
    * @param pTarget The target file of copying
    *
    * @throws IOException Throws if a file cannot be copied
    */
   public void copy(File pSource, File pTarget)
      throws IOException
   {
      if (pSource.isDirectory())
      {
         mkdir(pTarget);

         File[] files = pSource.listFiles();

         for (int i = files.length; i-- != 0;)
         {
            File file = files[i];
            copy(file, new File(pTarget.getPath() + "/" + file.getName()));
         }
      }
      else
      {
         remove(pTarget);

         File path = pTarget.getParentFile();

         if (path != null)
         {
            mkdir(path);
         }

         RandomAccessFile source = new RandomAccessFile(pSource, "r");
         RandomAccessFile target = new RandomAccessFile(pTarget, "rw");

         byte[] bytes = new byte[0x80000];
         int i;

         while ((i = source.read(bytes)) != -1)
         {
            target.write(bytes, 0, i);
         }

         target.close();
         source.close();

         println("copied: " + pSource.getPath() + " --> " + pTarget.getPath());
      }
   }

   /**
    * Moves a file to another file.
    *
    * @param pSource The source file of moving
    * @param pTarget The target file of moving
    *
    * @throws IOException Throws if a file cannot be moved
    */
   public void move(File pSource, File pTarget)
      throws IOException
   {
      if (pSource.isDirectory())
      {
         mkdir(pTarget);

         File[] files = pSource.listFiles();

         for (int i = files.length; i-- != 0;)
         {
            File file = files[i];
            move(file, new File(pTarget.getPath() + "/" + file.getName()));
         }

         remove(pSource);
      }
      else
      {
         remove(pTarget);

         File path = pTarget.getParentFile();

         if (path != null)
         {
            mkdir(path);
         }

         if (! pSource.renameTo(pTarget))
         {
            copy(pSource, pTarget);
            remove(pSource);
         }

         println("moved: " + pSource.getPath() + " --> " + pTarget.getPath());
      }
   }

   /**
    * Renames a file 
    *
    * @param pSource The source file of renaming
    * @param pTarget The target file of renaming
    *
    * @throws IOException Throws if a file cannot be renamed
    */
   public void rename(File pSource, File pTarget)
      throws IOException
   {
      move(pSource, pTarget);
   }

   /**
    * Removes a file 
    *
    * @param pFile The file which want be removed.
    *
    * @throws IOException Throws if a file cannot be removed
    */
   public void remove(File pFile)
      throws IOException
   {
      if (pFile.exists())
      {
         if (pFile.isDirectory())
         {
            File[] files = pFile.listFiles();

            for (int i = files.length; i-- != 0;)
            {
               remove(files[i]);
            }
         }

         if (! pFile.delete())
         {
            throw new IOException(cErrors[cError_Remove] + pFile);
         }

         println("removed: " + pFile.getPath());
      }
   }

   /**
    * Makes a directory
    *
    * @param pPath An directory path
    *
    * @throws IOException Throws if directory cannot be created.
    */
   public void mkdir(File pPath)
      throws IOException
   {
      if (! pPath.exists())
      {
         if (! pPath.mkdirs())
         {
            throw new IOException(cErrors[cError_MakeDirectories] +
               pPath.getPath());
         }

         println("created: " + pPath.getPath());
      }
   }

   /**
    * Invokes a parsed command 
    *
    * @param pCommand A string array of command
    *
    * @return An integer indicating whether the command
    *         was invoked successfully (0) or not (-1)
    */
   public int command(String[] pCommand)
   {
      return command(pCommand, null, null);
   }

   /**
    * Invokes a parsed command with an environment 
    *
    * @param pCommand A string array of command
    * @param pEnvironment A string array of environment
    *
    * @return An integer indicating whether the command
    *         was invoked successfully (0) or not (-1)
    */
   public int command(String[] pCommand, String[] pEnvironment)
   {
      return command(pCommand, pEnvironment, null);
   }

   /**
    * Invokes a parsed command in the specific working directory
    *
    * @param pCommand A string array of command
    * @param pWorkingDirectory the working directory for command
    *
    * @return An integer indicating whether the command
    *         was invoked successfully (0) or not (-1)
    */
   public int command(String[] pCommand, File pWorkingDirectory)
   {
      return command(pCommand, null, pWorkingDirectory);
   }

   /**
    * Invokes a parsed command with an environment and working directory.
    *
    * @param pCommand A string array of command
    * @param pEnvironment A string array of environment
    * @param pWorkingDirectory The working directory for command
    *
    * @return An integer indicating whether the command
    *         was invoked successfully (0) or not (-1)
    */
   public int command(String[] pCommand, String[] pEnvironment,
      File pWorkingDirectory)
   {
      try
      {
         // Check for an empty command
         if ((pCommand == null) || (pCommand.length == 0))
         {
            return 0;
         }

         // Invoke an intrinsic command
         try
         {
            try
            {
               Method method =
                  getClass()
                     .getDeclaredMethod("intrinsic_" + pCommand[0],
                     new Class[]{ java.lang.String[].class });

               method.invoke(this, new Object[]{ pCommand });

               return 0;
            }
            catch (InvocationTargetException ite)
            {
               throw ite.getTargetException();
            }
         }
         catch (NoSuchMethodException nsme)
         {
         }

         // Invoke an external command process
         Process process = null;

         try
         {
            // Prepare the context
            String alias = pCommand[0];
            String[] command = (String[]) mContext.get(alias);

            if (command != null)
            {
               List<String> list =
                  new ArrayList<String>(Arrays.asList(pCommand));
               list.remove(0);
               list.addAll(0, Arrays.asList(command));

               pCommand = (String[]) list.toArray(new String[0]);
            }

            if (pEnvironment == null)
            {
               pEnvironment = (String[]) mContext.get(alias + ".environment");
            }

            if (pWorkingDirectory == null)
            {
               pWorkingDirectory = (File) mContext.get(alias + ".directory");
            }

            // Exec the process
            process = Runtime.getRuntime()
                             .exec(pCommand, pEnvironment, pWorkingDirectory);

            Pipe stdin = new Pipe(mStdin, process.getOutputStream());

            Pipe stdout =
               new Pipe(process.getInputStream(), mSilent ? null : mOutputStream);
            Pipe stderr =
               new Pipe(process.getErrorStream(), mSilent ? null : mErrorStream);
            stdin.start();
            stdout.start();
            stderr.start();

            process.waitFor();

            while (stdin.isAlive() || stdout.isAlive() || stderr.isAlive())
            {
               Thread.yield();
            }

            return process.exitValue();
         }
         catch (InterruptedException ie)
         {
            if (process != null)
            {
               process.destroy();
            }
         }
      }
      catch (Throwable t)
      {
         mStderr.println("\n" + t);
      }

      return -1;
   }

   /**
    * Invokes an command
    *
    * @param pCommand A command string
    *
    * @return An integer indicating whether the command
    *         was invoked successfully (0) or not (-1)
    */
   public int command(String pCommand)
   {
      return command(pCommand, null, null);
   }

   /**
    * Invokes an command with an environment
    *
    * @param pCommand A command string
    * @param pEnvironment An string array of environment
    *
    * @return An integer indicating whether the command
    *         was invoked successfully (0) or not (-1)
    */
   public int command(String pCommand, String[] pEnvironment)
   {
      return command(pCommand, pEnvironment, null);
   }

   /**
    * Invokes a command in the working directory
    *
    * @param pCommand A command string
    * @param pWorkingDirectory An working directory
    *
    * @return An integer indicating whether the command
    *         was invoked successfully (0) or not (-1)
    */
   public int command(String pCommand, File pWorkingDirectory)
   {
      return command(pCommand, null, pWorkingDirectory);
   }

   /**
    * Invokes a command in the working directory with environment
    *
    * @param pCommand A command string
    * @param pEnvironment A string array of environment
    * @param pWorkingDirectory A working directory
    *
    * @return An integer indicating whether the command
    *         was invoked successfully (0) or not (-1)
    */
   public int command(String pCommand, String[] pEnvironment,
      File pWorkingDirectory)
   {
      return command(parse(pCommand), pEnvironment, pWorkingDirectory);
   }

   // Intrinsic commands =====================================================
   /**
    * Intrinsic echo command
    *
    * @param pArguments A string arrary of arguments
    *
    * @throws Throwable Throws if echo command is not working
    */
   public void intrinsic_echo(String[] pArguments)
      throws Throwable
   {
      echo(pArguments[1]);
   }

   /**
    * Intrinsic extract command
    *
    * @param pArguments A string array of arguments
    *
    * @throws Throwable Throws if extract command is not working
    */
   public void intrinsic_extract(String[] pArguments)
      throws Throwable
   {
      extract(new JarFile(pArguments[1]), new File(pArguments[2]));
   }

   /**
    * intrinsic command of copy
    *
    * @param pArguments string array of arguments
    *
    * @throws Throwable all errors and exceptions
    */
   public void intrinsic_copy(String[] pArguments)
      throws Throwable
   {
      copy(new File(pArguments[1]), new File(pArguments[2]));
   }

   /**
    * Intrinsic move command
    *
    * @param pArguments A string array of arguments
    *
    * @throws Throwable Throws if move command is not working
    */
   public void intrinsic_move(String[] pArguments)
      throws Throwable
   {
      move(new File(pArguments[1]), new File(pArguments[2]));
   }

   /**
    * Intrinsic rename command
    *
    * @param pArguments A string array of arguments
    *
    * @throws Throwable Throws if rename command is not working 
    */
   public void intrinsic_rename(String[] pArguments)
      throws Throwable
   {
      intrinsic_rename(pArguments);
   }

   /**
    * Intrinsic command of remove
    *
    * @param pArguments String array of arguments which is command
    *
    * @throws Throwable Throws if remove command is not working
    */
   public void intrinsic_remove(String[] pArguments)
      throws Throwable
   {
      for (int i = 1; i < pArguments.length; ++i)
      {
         remove(new File(pArguments[i]));
      }
   }

   /**
    * Intrinsic mkdirs command
    *
    * @param pArguments A string array of arguments which is command
    *
    * @throws Throwable Throws if the directory can not be made
    */
   public void intrinsic_mkdirs(String[] pArguments)
      throws Throwable
   {
      for (int i = 1; i < pArguments.length; ++i)
      {
         mkdir(new File(pArguments[i]));
      }
   }

   /**
    * Connects output stream and input stream
    */
   private class Pipe
      extends Thread
   {
      /**
       * Holds an input stream
       */
      private InputStream mInputStream;

      /**
       * Holds an output stream
       */
      private OutputStream mOutputStream;

      /**
       * Creates a new Pipe object and set this as deamon thread
       *
       * @param pInputStream An input stream
       * @param pOutputStream An output stream
       *
       * @throws Throwable Throws if an input stream and an output stream
       *                   are not valid
       */
      private Pipe(InputStream pInputStream, OutputStream pOutputStream)
         throws Throwable
      {
         mInputStream = pInputStream;
         mOutputStream = pOutputStream;

         setDaemon(true);
      }

      /**
       * Writes an input stream into output stream.
       */
      public void run()
      {
         try
         {
            int i;

            if (mOutputStream == null)
            {
               while (mInputStream.read() != -1)
                  ;
            }
            else
            {
               while ((i = mInputStream.read()) != -1)
               {
                  mOutputStream.write(i);
               }

               mOutputStream.flush();
            }
         }
         catch (Throwable t)
         {
         }
      }
   }
}
