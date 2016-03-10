
df = readLines("rules.out", n = -1)
print(sprintf("Number of total occurrences of RULES: %d", length(df)))

df = as.data.frame(df)
df$df = as.character(df$df)
df$uri = NA
df$line = NA
df$rule = NA
df$id = NA
df$ver = NA
df$verkey = NA
df$pkg = NA
df$file = NA

i = 193
for (i in 1:nrow(df)) {
  s = df$df[i]
  sp = strsplit(s, ':')
  uri = sp[[1]][1]
  line = sp[[1]][2]
  rule = sp[[1]][3]
  id = strsplit(uri, '/')[[1]][2]
  idsp = strsplit(id, '-')
  
  ver = idsp[[1]][length(idsp[[1]])]
  versp = strsplit(ver, '\\.')[[1]]
  verkey = 0
  v = 2
  for (v in length(versp):1) {
    verkey = verkey + as.numeric(versp[v]) * (100^(length(versp)-v))
  }
  
  pkg = paste(idsp[[1]][1: length(idsp[[1]])-1 ], collapse='-')
  file = paste(strsplit(uri, '/')[[1]][-1][-1], collapse='/')
  
  df$uri[i] = uri
  df$line[i] = line
  df$rule[i] = rule
  df$id[i] = id
  df$ver[i] = ver
  df$verkey[i] = verkey
  df$pkg[i] = pkg
  df$file[i] = file
}

df$df = NULL

df$pkg = factor(df$pkg)

print(sprintf("Number of packages using GHC RULES: %d", length(levels(df$pkg))))

library('sqldf')

df = sqldf('select uri, line, rule, pkg, file, ver, max(verkey) as verkey, count(*) as c from df group by pkg, file order by count(*) desc')
df$line = NULL
df$rule = NULL
df$pkg = NULL
df$file = NULL
df$ver = NULL
df$verkey = NULL
df$c = NULL

print(sprintf("Number of packages/files using GHC RULES: %d", nrow(df)))

write.table(df,file="rules.csv", row.names=FALSE, sep=",")
