package org.firstthreeodds.system;

import org.slf4j.*;

/**
 * Serves to wrap the simple implementation of org.slf4j.Logger that sends
 * all enabled log messages, for all defined loggers, to the configured
 * destination (either the console (System.err) or a specified file).
 *
 * All calls that would have looked like:
 *
 *     final Logger logger = LoggerFactory.getLogger(SomeClass.class);
 *     logger.info("Some Message");
 *
 * will instead be invoked as:
 *
 *     info(this, "Some Message");
 *
 * where "this" is an instance of SomeClass.
 *
 * LoggerService's methods must be statically imported for this to work:
 *
 * import static thirstifish.system.LoggerService.*;
 *
 * Convenience methods allow a Class instead of an Object to be passed
 * as the first parameter, to allow logging from static contexts (where
 * no "this" is available).
 */
public class LoggerService
{
   /**
    * A simple implementation that logs messages of level TRACE.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    */
   public static void trace(Object pObject, String pMessage)
   {
      trace(pObject.getClass(), pMessage);
   }

   /**
    * Performs single parameter substitution before logging
    * the message of level TRACE.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the parameter to substitute.
    */
   public static void trace(Object pObject, String pFormat, Object pParam1)
   {
      trace(pObject.getClass(), pFormat, pParam1);
   }

   /**
    * Performs double parameter substitution before logging
    * the message of level TRACE.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the first parameter to substitute.
    *
    * @param pParam2 the second parameter to substitute.
    */
   public static void trace(Object pObject, String pFormat,
                            Object pParam1, Object pParam2)
   {
      trace(pObject.getClass(), pFormat, pParam1, pParam2);
   }

   /**
    * Performs multiple parameter substitution before logging
    * the message of level TRACE.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParams the multiple parameters to substitute.
    */
   public static void trace(Object pObject, String pFormat, Object... pParams)
   {
      trace(pObject.getClass(), pFormat, pParams);
   }

   /**
    * Logs a message of level TRACE, including an exception.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    *
    * @param pThrowable the exception to be logged.
    */
   public static void trace(Object pObject, String pMessage,
                            Throwable pThrowable)
   {
      trace(pObject.getClass(), pMessage, pThrowable);
   }

   /**
    * A simple implementation that logs messages of level TRACE.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    */
   public static void trace(Class pClass, String pMessage)
   {
      LoggerFactory.getLogger(pClass).trace(pMessage);
   }

   /**
    * Performs single parameter substitution before logging
    * the message of level TRACE.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the parameter to substitute.
    */
   public static void trace(Class pClass, String pFormat, Object pParam1)
   {
      LoggerFactory.getLogger(pClass).trace(pFormat, pParam1);
   }

   /**
    * Performs double parameter substitution before logging
    * the message of level TRACE.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the first parameter to substitute.
    *
    * @param pParam2 the second parameter to substitute.
    */
   public static void trace(Class pClass, String pFormat,
                            Object pParam1, Object pParam2)
   {
      LoggerFactory.getLogger(pClass).trace(pFormat, pParam1, pParam2);
   }

   /**
    * Performs multiple parameter substitution before logging
    * the message of level TRACE.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParams the multiple parameters to substitute.
    */
   public static void trace(Class pClass, String pFormat, Object... pParams)
   {
      LoggerFactory.getLogger(pClass).trace(pFormat, pParams);
   }

   /**
    * Logs a message of level TRACE, including an exception.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    *
    * @param pThrowable the exception to be logged.
    */
   public static void trace(Class pClass, String pMessage,
                            Throwable pThrowable)
   {
      LoggerFactory.getLogger(pClass).trace(pMessage, pThrowable);
   }

   /**
    * A simple implementation that logs messages of level DEBUG.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    */
   public static void debug(Object pObject, String pMessage)
   {
      debug(pObject.getClass(), pMessage);
   }

   /**
    * Performs single parameter substitution before logging
    * the message of level DEBUG.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the parameter to substitute.
    */
   public static void debug(Object pObject, String pFormat, Object pParam1)
   {
      debug(pObject.getClass(), pFormat, pParam1);
   }

   /**
    * Performs double parameter substitution before logging
    * the message of level DEBUG.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the first parameter to substitute.
    *
    * @param pParam2 the second parameter to substitute.
    */
   public static void debug(Object pObject, String pFormat,
                            Object pParam1, Object pParam2)
   {
      debug(pObject.getClass(), pFormat, pParam1, pParam2);
   }

   /**
    * Performs multiple parameter substitution before logging
    * the message of level DEBUG.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParams the multiple parameters to substitute.
    */
   public static void debug(Object pObject, String pFormat, Object... pParams)
   {
      debug(pObject.getClass(), pFormat, pParams);
   }

   /**
    * Logs a message of level DEBUG, including an exception.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    *
    * @param pThrowable the exception to be logged.
    */
   public static void debug(Object pObject, String pMessage,
                            Throwable pThrowable)
   {
      debug(pObject.getClass(), pMessage, pThrowable);
   }

   /**
    * A simple implementation that logs messages of level DEBUG.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    */
   public static void debug(Class pClass, String pMessage)
   {
      LoggerFactory.getLogger(pClass).debug(pMessage);
   }

   /**
    * Performs single parameter substitution before logging
    * the message of level DEBUG.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the parameter to substitute.
    */
   public static void debug(Class pClass, String pFormat, Object pParam1)
   {
      LoggerFactory.getLogger(pClass).debug(pFormat, pParam1);
   }

   /**
    * Performs double parameter substitution before logging
    * the message of level DEBUG.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the first parameter to substitute.
    *
    * @param pParam2 the second parameter to substitute.
    */
   public static void debug(Class pClass, String pFormat,
                            Object pParam1, Object pParam2)
   {
      LoggerFactory.getLogger(pClass).debug(pFormat, pParam1, pParam2);
   }

   /**
    * Performs multiple parameter substitution before logging
    * the message of level DEBUG.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParams the multiple parameters to substitute.
    */
   public static void debug(Class pClass, String pFormat, Object... pParams)
   {
      LoggerFactory.getLogger(pClass).debug(pFormat, pParams);
   }

   /**
    * Logs a message of level DEBUG, including an exception.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    *
    * @param pThrowable the exception to be logged.
    */
   public static void debug(Class pClass, String pMessage,
                            Throwable pThrowable)
   {
      LoggerFactory.getLogger(pClass).debug(pMessage, pThrowable);
   }

   /**
    * A simple implementation that logs messages of level INFO.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    */
   public static void info(Object pObject, String pMessage)
   {
      info(pObject.getClass(), pMessage);
   }

   /**
    * Performs single parameter substitution before logging
    * the message of level INFO.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the parameter to substitute.
    */
   public static void info(Object pObject, String pFormat, Object pParam1)
   {
      info(pObject.getClass(), pFormat, pParam1);
   }

   /**
    * Performs double parameter substitution before logging
    * the message of level INFO.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the first parameter to substitute.
    *
    * @param pParam2 the second parameter to substitute.
    */
   public static void info(Object pObject, String pFormat,
                           Object pParam1, Object pParam2)
   {
      info(pObject.getClass(), pFormat, pParam1, pParam2);
   }

   /**
    * Performs multiple parameter substitution before logging
    * the message of level INFO.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParams the multiple parameters to substitute.
    */
   public static void info(Object pObject, String pFormat, Object... pParams)
   {
      info(pObject.getClass(), pFormat, pParams);
   }

   /**
    * Logs a message of level INFO, including an exception.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    *
    * @param pThrowable the exception to be logged.
    */
   public static void info(Object pObject, String pMessage,
                           Throwable pThrowable)
   {
      info(pObject.getClass(), pMessage, pThrowable);
   }

   /**
    * A simple implementation that logs messages of level INFO.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    */
   public static void info(Class pClass, String pMessage)
   {
      LoggerFactory.getLogger(pClass).info(pMessage);
   }

   /**
    * Performs single parameter substitution before logging
    * the message of level INFO.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the parameter to substitute.
    */
   public static void info(Class pClass, String pFormat, Object pParam1)
   {
      LoggerFactory.getLogger(pClass).info(pFormat, pParam1);
   }

   /**
    * Performs double parameter substitution before logging
    * the message of level INFO.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the first parameter to substitute.
    *
    * @param pParam2 the second parameter to substitute.
    */
   public static void info(Class pClass, String pFormat,
                           Object pParam1, Object pParam2)
   {
      LoggerFactory.getLogger(pClass).info(pFormat, pParam1, pParam2);
   }

   /**
    * Performs multiple parameter substitution before logging
    * the message of level INFO.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParams the multiple parameters to substitute.
    */
   public static void info(Class pClass, String pFormat, Object... pParams)
   {
      LoggerFactory.getLogger(pClass).info(pFormat, pParams);
   }

   /**
    * Logs a message of level INFO, including an exception.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    *
    * @param pThrowable the exception to be logged.
    */
   public static void info(Class pClass, String pMessage,
                           Throwable pThrowable)
   {
      LoggerFactory.getLogger(pClass).info(pMessage, pThrowable);
   }

   /**
    * A simple implementation that logs messages of level WARN.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    */
   public static void warn(Object pObject, String pMessage)
   {
      warn(pObject.getClass(), pMessage);
   }

   /**
    * Performs single parameter substitution before logging
    * the message of level WARN.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the parameter to substitute.
    */
   public static void warn(Object pObject, String pFormat, Object pParam1)
   {
      warn(pObject.getClass(), pFormat, pParam1);
   }

   /**
    * Performs double parameter substitution before logging
    * the message of level WARN.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the first parameter to substitute.
    *
    * @param pParam2 the second parameter to substitute.
    */
   public static void warn(Object pObject, String pFormat,
                           Object pParam1, Object pParam2)
   {
      warn(pObject.getClass(), pFormat, pParam1, pParam2);
   }

   /**
    * Performs multiple parameter substitution before logging
    * the message of level WARN.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParams the multiple parameters to substitute.
    */
   public static void warn(Object pObject, String pFormat, Object... pParams)
   {
      warn(pObject.getClass(), pFormat, pParams);
   }

   /**
    * Logs a message of level WARN, including an exception.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    *
    * @param pThrowable the exception to be logged.
    */
   public static void warn(Object pObject, String pMessage,
                           Throwable pThrowable)
   {
      warn(pObject.getClass(), pMessage, pThrowable);
   }

   /**
    * A simple implementation that logs messages of level WARN.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    */
   public static void warn(Class pClass, String pMessage)
   {
      LoggerFactory.getLogger(pClass).warn(pMessage);
   }

   /**
    * Performs single parameter substitution before logging
    * the message of level WARN.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the parameter to substitute.
    */
   public static void warn(Class pClass, String pFormat, Object pParam1)
   {
      LoggerFactory.getLogger(pClass).warn(pFormat, pParam1);
   }

   /**
    * Performs double parameter substitution before logging
    * the message of level WARN.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the first parameter to substitute.
    *
    * @param pParam2 the second parameter to substitute.
    */
   public static void warn(Class pClass, String pFormat,
                           Object pParam1, Object pParam2)
   {
      LoggerFactory.getLogger(pClass).warn(pFormat, pParam1, pParam2);
   }

   /**
    * Performs multiple parameter substitution before logging
    * the message of level WARN.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParams the multiple parameters to substitute.
    */
   public static void warn(Class pClass, String pFormat, Object... pParams)
   {
      LoggerFactory.getLogger(pClass).warn(pFormat, pParams);
   }

   /**
    * Logs a message of level WARN, including an exception.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    *
    * @param pThrowable the exception to be logged.
    */
   public static void warn(Class pClass, String pMessage,
                           Throwable pThrowable)
   {
      LoggerFactory.getLogger(pClass).warn(pMessage, pThrowable);
   }

   /**
    * A simple implementation that logs messages of level ERROR.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    */
   public static void error(Object pObject, String pMessage)
   {
      error(pObject.getClass(), pMessage);
   }

   /**
    * Performs single parameter substitution before logging
    * the message of level ERROR.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the parameter to substitute.
    */
   public static void error(Object pObject, String pFormat, Object pParam1)
   {
      error(pObject.getClass(), pFormat, pParam1);
   }

   /**
    * Performs double parameter substitution before logging
    * the message of level ERROR.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the first parameter to substitute.
    *
    * @param pParam2 the second parameter to substitute.
    */
   public static void error(Object pObject, String pFormat,
                            Object pParam1, Object pParam2)
   {
      error(pObject.getClass(), pFormat, pParam1, pParam2);
   }

   /**
    * Performs multiple parameter substitution before logging
    * the message of level ERROR.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParams the multiple parameters to substitute.
    */
   public static void error(Object pObject, String pFormat, Object... pParams)
   {
      error(pObject.getClass(), pFormat, pParams);
   }

   /**
    * Logs a message of level ERROR, including an exception.
    *
    * @param pObject the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    *
    * @param pThrowable the exception to be logged.
    */
   public static void error(Object pObject, String pMessage,
                            Throwable pThrowable)
   {
      error(pObject.getClass(), pMessage, pThrowable);
   }

   /**
    * A simple implementation that logs messages of level ERROR.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    */
   public static void error(Class pClass, String pMessage)
   {
      LoggerFactory.getLogger(pClass).error(pMessage);
   }

   /**
    * Performs single parameter substitution before logging
    * the message of level ERROR.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the parameter to substitute.
    */
   public static void error(Class pClass, String pFormat, Object pParam1)
   {
      LoggerFactory.getLogger(pClass).error(pFormat, pParam1);
   }

   /**
    * Performs double parameter substitution before logging
    * the message of level ERROR.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParam1 the first parameter to substitute.
    *
    * @param pParam2 the second parameter to substitute.
    */
   public static void error(Class pClass, String pFormat,
                            Object pParam1, Object pParam2)
   {
      LoggerFactory.getLogger(pClass).error(pFormat, pParam1, pParam2);
   }

   /**
    * Performs multiple parameter substitution before logging
    * the message of level ERROR.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pFormat the formatted string to be substituted into.
    *
    * @param pParams the multiple parameters to substitute.
    */
   public static void error(Class pClass, String pFormat, Object... pParams)
   {
      LoggerFactory.getLogger(pClass).error(pFormat, pParams);
   }

   /**
    * Logs a message of level ERROR, including an exception.
    *
    * @param pClass the class of the object to which the logger is keyed.
    *
    * @param pMessage the message to be logged.
    *
    * @param pThrowable the exception to be logged.
    */
   public static void error(Class pClass, String pMessage,
                            Throwable pThrowable)
   {
      LoggerFactory.getLogger(pClass).error(pMessage, pThrowable);
   }
}
