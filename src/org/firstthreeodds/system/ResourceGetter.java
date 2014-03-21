package org.firstthreeodds.system;

import java.io.*;

import java.net.*;

import java.util.zip.*;

/**
 * Gets resources as streams or strings.
 *
 * @author Rick Neff
 */
public class ResourceGetter
{
   /**
    * The path prefix for property-like resources.
    * The leading slash ('/') guarantees that the
    * cResourcesPathPrefix lookup will work regardless
    * of deployment scenario.
    *
    * However, for command-line invocation, the project home
    * directory, or current directory (".") if connected to
    * the project home directory, must be in the CLASSPATH.
    * That is the directory where the "resources" subdirectory
    * is located.
    */
   public static final String cResourcesPathPrefix = "/resources/properties/";

   /**
    * Encapsulates getting a resource given its string basename.
    *
    * @param pName the string basename of the resource.
    *
    * @return an input stream opened on the resource URL.
    */
   public static InputStream getResourceAsStream(String pName)
   {
      return getResourceAsStream(ResourceGetter.class, pName);
   }

   /**
    * Encapsulates getting a resource given its string basename.
    *
    * @param pClass the class of the resource (defaults to this class).
    *
    * @param pName the string basename of the resource.
    *
    * @return an input stream opened on the resource URL.
    */
   public static InputStream getResourceAsStream(Class pClass, String pName)
   {
      InputStream inputStream = null;

      try
      {
         inputStream = new URL(getResource(pClass, pName)).openStream();
      }
      catch (Exception e)
      {
         System.err.println(e);
      }

      return inputStream;
   }

   /**
    * Encapsulates getting a resource given its string basename.
    *
    * @param pName the string basename of the resource.
    *
    * @return a String external form of the resource URL.
    */
   public static String getResource(String pName)
   {
      return getResource(ResourceGetter.class, pName);
   }

   /**
    * Encapsulates getting a resource given its class and its string basename.
    *
    * @param pClass the class of the resource (defaults to this class).
    *
    * @param pName the string basename of the resource.
    *
    * @return a String external form of the resource URL.
    */
   public static String getResource(Class pClass, String pName)
   {
      String externalForm = "Sorry---Resource_Not_Found---" + pName;

      String resourceFilename = (pName.startsWith("/") ? pName :
                                 cResourcesPathPrefix + pName);
      try
      {
         externalForm = 
            pClass.getResource(resourceFilename).toExternalForm();
      }
      catch (Exception e)
      {
      }

      return externalForm;
   }

   /**
    * Extracts a file or files from a URL-retrieved web resource
    * and saves it/them to the local filesystem.
    *
    * @param pSpecification the URL specification.
    * @param pFoldername the name of the folder in which to save it,
                         which must already exist.
    * @param pFilenames the filenames in the URL resource.
    */
   public static void extractToFile(String pSpecification,
                                    String pFoldername,
                                    String ... pFilenames)
   {
      try
      {
         URL url = new URL(pSpecification);
         ZipInputStream source = new ZipInputStream(url.openStream());
         File folder = new File(pFoldername);
         byte[] bytes = new byte[0x80000];

         for (ZipEntry ze; (ze = source.getNextEntry()) != null;)
         {
            for (int i = 0; i < pFilenames.length; i++)
            {
               String filename = pFilenames[i];
               if (ze.getName().equals(filename))
               {
                  File file = new File(folder, new File(filename).getName());
                  RandomAccessFile target = new RandomAccessFile(file, "rw");
                  int numRead;

                  while ((numRead = source.read(bytes)) != -1)
                  {
                     target.write(bytes, 0, numRead);
                  }
                  target.close();
               }
            }
         }
         source.close();
      }
      catch (Exception e)
      {
         System.out.println(e);
      }
   }
}
