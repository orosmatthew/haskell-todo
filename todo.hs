import Data.Char

addTask tasks = do
  putStrLn "Enter Task to Add:"
  line <- getLine
  let newTasks = tasks ++ [line]
  return newTasks

printTasks tasks = do
  putStrLn "Here are your tasks:"
  doPrint tasks
  where
    doPrint [] = return ()
    doPrint (h : t) = do
      putStrLn ("\t" ++ h)
      doPrint t

searchTask tasks = do
  putStrLn "Enter Task To Search:"
  line <- getLine
  search tasks line
  where
    search [] task = do
      putStrLn ("Could not find " ++ task)
    search (h : t) task = do
      if h == task then putStrLn ("Found " ++ task) else search t task

doOption opt tasks
  | opt == "add" = addTask tasks
  | opt == "print" = do
      printTasks tasks
      return tasks
  | opt == "search" = do
      searchTask tasks
      return tasks
  | otherwise = do
      putStrLn "Error"
      return tasks

doMenu tasks = do
  putStrLn "Below are the options:"
  putStrLn "\tadd"
  putStrLn "\tprint"
  putStrLn "\tsearch"
  putStrLn "Enter option:"
  line <- getLine
  newTasks <- doOption (map toLower line) tasks
  doMenu newTasks

main = do
  putStrLn "Welcome to Haskell Task Manager"
  doMenu []