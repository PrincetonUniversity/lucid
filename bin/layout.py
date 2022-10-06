#!python3

import json, re, sys
import networkx as nx
from collections import namedtuple


def main():
  infn = sys.argv[1]
  print (infn)
  dg, groups = build_dependency_graph("learner.json")
  pipe = layout(dg, groups)
  return

Array = namedtuple("Array", "id ty size")
Variable = namedtuple("Variable", "id ty size")
Statement = namedtuple("Statement", "id statement user_table resources")

def arr_from_json(js): 
  return Array(js["id"], js["ty"], js["size"])
def var_from_json(js):
  return Variable(js["id"], js["ty"], js["size"])

# make a dictionary of all the resource that this statement uses
def stmt_resources(stmt, vars, arrs):
  r_any_op = r'\n(.*?);'
  ops = re.findall(r_any_op, stmt)
  array_ops, hash_ops, alu_ops = [], [], []
  for op in ops:
    if ("Array" in op):
      array_ops.append(op)
    elif ("hash" in op):
      hash_ops.append(op)
    else:
      alu_ops.append(op)

  array_ids = list(set(re.findall(r'.*Array..\.update.*?\((.*?),', stmt)))
  match_str = re.findall(r'match (.*?) with', stmt)[0]
  match_str = match_str.replace("(", "").replace(")", "").strip()
  match_vars = []
  if (match_str != ""):
    match_vars = [mv.strip() for mv in match_str.split(",")]
  match_vars = [vars[id] for id in match_vars]
  return {
    "arrays":[arrs[aid] for aid in array_ids],
    "keys":match_vars,
    "array_ops":array_ops, 
    "hash_ops":hash_ops, 
    "alu_ops":alu_ops
    }

def stmt_from_json(js, vars, arrs):
  stmt = js["statement"]
  user_table = False
  if (js["user_table"] == "true"):
    user_table = True
  return Statement(js["id"], stmt, user_table, stmt_resources(stmt, vars, arrs))


# we are laying out groups / bundles of statement. Each bundle contains 
# all the statements that access a particular register array.

def groupid_of_stmt(s):
  if (len(s.resources["arrays"]) == 1):
    return s.resources["arrays"][0].id
  elif (len(s.resources["arrays"]) == 0):
    return "stmt~%s"%(s.id)
  else:
    print ("error -- statement accesses more than 1 array. Impossible.")
    exit(1)

class StatementGroup (object):
  """ A group of statements that must go into the same stage
      (either because that all use the same array, or because 
      the layout algorithm has placed them in the same table) """  
  def __init__(self, gid):
    self.gid = gid
    self.statements = []
    self.user_table = False
    # resources required by this statement group
    self.resources = {
      "keybits":0,
      "sram_blocks":0,
      "arrays":0,
      "hash_ops":0,
      "array_ops":0
    }

  def calc_keybits(self):
    keys = []
    for stmt in self.statements:
      keys += stmt.resources["keys"]
    keys = list(set(keys))
    self.resources["keybits"] = sum([mv.size for mv in keys])

  def calc_sram(self):
    sram_block_size = 1024 * 128 # 128 kb
    arrays = []
    for stmt in self.statements:
      arrays += stmt.resources["arrays"]
    arrays = list(set(arrays))
    self.resources["sram_blocks"] = sum([(array.size // sram_block_size) + 2 for array in arrays])

  def count_resource(self, resource):
    units = []
    for stmt in self.statements:
      units += stmt.resources[resource]
    self.resources[resource] = len(list(set(units)))

  def calc_resources(self):
    """ calculate this statement groups resource footprint """
    self.calc_keybits()
    self.calc_sram()
    self.count_resource("arrays")
    self.count_resource("hash_ops")
    self.count_resource("array_ops")

  def add_stmt(self, stmt):
    self.statements = self.statements + [stmt]
    self.user_table = self.user_table or stmt.user_table
    # update resources
    self.calc_resources()

  def node_key(self):
    return (self.gid)#, {"obj":self})

  def resource_summary(self):
    rstr = "statements : %i"%len(self.statements)
    for (r, v) in self.resources.items():
      rstr+="\n%s : %i"%(r, v)
    return rstr


class Dependency(object):
  def __init__(self, srcid, dstid):
    self.srcid = srcid
    self.dstid = dstid
    self.dep_tys = []
  def add_dep_ty(self, dep_ty):
    self.dep_tys = self.dep_tys + [dep_ty]
  def edge_key(self):
    return (self.srcid, self.dstid)#, {"obj":self})

def build_dependency_graph(json_fn):
  prog_json = json.load(open(json_fn, "r"))
  arrs = {arr["id"]:arr_from_json(arr) for arr in prog_json["arrays"]}
  vars = {v["id"]:var_from_json(v) for v in prog_json["vars"]}
  stmts = {st["id"]:stmt_from_json(st, vars, arrs) for st in prog_json["statements"]}
  # generate statement groups
  groups = {}  # array or stmt id -> group
  for s in stmts.values():
    group_id = groupid_of_stmt(s)
    group = groups.get(group_id, StatementGroup(group_id))
    group.add_stmt(s)
    groups[group_id] = group
  # find dependencies between statement groups 
  deps = {} # (src, dst) -> dep obj
  for dep in prog_json["dependencies"]:
    src_group_id = groupid_of_stmt(stmts[dep["srcid"]])
    dst_group_id = groupid_of_stmt(stmts[dep["dstid"]])
    key = (src_group_id, dst_group_id)
    d = deps.get(key, Dependency(src_group_id, dst_group_id))
    d.add_dep_ty(dep["dep_ty"])
    deps[key] = d

  dg = nx.DiGraph()
  dg.add_nodes_from([sg.node_key() for sg in groups.values()])
  dg.add_edges_from([dep.edge_key() for dep in deps.values()])
  return dg, groups

# find the minimum stage for each group based
# on previous dependencies. 
def min_stage_from_dependencies(group, dg, groups):
  min_stage = 0
  for pred_gid in dg.predecessors(group.gid):
    pred_group = groups[pred_gid]
    if (pred_group.stage == None):
      min_stage = None
    elif (min_stage == None):
      min_stage = None
    else:
      min_stage = max(min_stage, (pred_group.stage+1))
  return min_stage




# combine 2 statement groups
def merge_statement_groups(sg1, sg2):
  new_g = StatementGroup(sg1.gid)
  new_g.statements = sg1.statements + sg2.statements
  for s in new_g.statements:
    new_g.user_table = new_g.user_table or s.user_table
  new_g.calc_resources()
  return new_g

# check if a statement group obeys a list of constraints
def check_statement_group(sg, constraints):
  for resource, limit in constraints.items():
    if (sg.resources[resource] > limit):
      return False
  return True


# A simple resource model of the tofino pipeline: 
# a pipeline of stages, each stage has tables. 
# tables implement statements 
class Table(object):
  """ A physical tofino table """
  def __init__(self, table_id):
    # resource constraints
    self.constraints = {
      "keybits" : 512,
      "arrays" : 1,
      "sram_blocks": 35,
      "hash_ops" : 1,
      "array_ops": 4
    }
    # contents
    self.table_id = table_id
    self.active = False
    self.statement_group = StatementGroup("table%i"%self.table_id) 

  def add_group(self, sg):
    """ place the statement group sg in this table. If it violates 
        a resource constraint, return false """
    # cannot merge two user tables
    if (self.statement_group.user_table and sg.user_table):
      return False
    new_g = merge_statement_groups(self.statement_group, sg)
    if (check_statement_group(new_g, self.constraints)):
      self.statement_group = new_g
      self.active = True
      return True
    else:
      return False

  def resource_summary(self):
    rstr = "-- table %s resources --\n"%(self.table_id)
    rstr +=self.statement_group.resource_summary()
    return rstr

class Stage(object):
  """ A physical tofino stage """
  def __init__(self, stage_id, n_tables):
    self.stage_id = stage_id
    self.constraints = {
      "arrays" : 4,
      "sram_blocks": 48,
      "hash_ops" : 6,
      "array_ops": 4
    }
    self.tables = [Table(i) for i in range(n_tables)]      
  # left off here: 
  # what is the relationship between table resources and stage 
  # resources? Given a new statement group, can you tell whether the 
  # table will fit into the stage without merging it into a table?
  def add_group(self, sg):
    loc = None
    for t in self.tables:
      success = t.add_group(sg)
      if success:
        loc = (self.stage_id, t.table_id)
        print ("placed in (stage, table): (%i, %i)"%loc)
        break
    return loc

class Pipeline(object):
  """ The tofino match-action pipeline """
  def __init__(self, n_stages, n_tables):
    # contents
    self.stages = [Stage(i, n_tables) for i in range(n_stages)]
  def add_group(self, sg, min_stage):
    for s in self.stages[min_stage::]:
      loc = s.add_group(sg)
      if loc is not None:
        return loc
    return None

def layout(dg, groups):
  pipe = Pipeline(12, 16)
  print ("**** starting layout ****")  
  # layout can be done in a single topologically 
  # ordered pass through the statement groups
  for gid in nx.topological_sort(dg):
    print ("placing: %s"%gid)
    dependency_stage = min_stage_from_dependencies(groups[gid], dg, groups)
    stage, table = pipe.add_group(groups[gid], dependency_stage)
    groups[gid].stage=stage

  print ("**** layout finished ****")
  for s in pipe.stages:
    n_active = len([t for t in s.tables if t.active])
    if (n_active):
      print ("---- stage %i [%i tables] ----"%(s.stage_id, n_active))
      for t in s.tables:
        if (t.active):
          print (t.resource_summary())

if __name__ == '__main__':
  main()
