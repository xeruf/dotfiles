{

dataLocation = "~/.local/share/task",

uda = [
  {
    name = "url",
    type = "string",
    label = "URL"
  },
  {
    name = "recurDue",
    type = "duration",
    label = "RecDue"
  {
    name = "recurWait",
    type = "duration",
    label = "RecWait"
  },
]

context = [
  {
    name = "dev",
    filter = "+dev"
  },
  {
    name = "work",
    filter = "+work"
  },
  {
    name = "fog",
    filter = "+comm or +admin or +move or +config or +browse"
  },
]

report = [
  {
    name = "inbox"
    description = "Relevant tasks without project",
    columns = "id,priority,tags,scheduled.countdown,due.relative,description.desc,url,urgency",
    labels = "ID,Prio,Tags,⏰,Description,Url,Urg",
    filter = "status:pending project: -config -consume",
    sort = "urgency-"
  },
  {
    name = "next",
    columns = "id,depends,priority,tags,scheduled.relative,due.relative,project,description.count,urgency",
    labels = "ID,Deps,Prio,Tags,,⏰,Proj,Description,Urg",
    filter = "status:pending limit:20"
  }
]

}
# i lli "A"n
