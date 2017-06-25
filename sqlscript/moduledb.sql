create table module_component_values (
  module text not null,
  component text not null,
  node text NULL,
  data bytea not null,
  PRIMARY KEY (module, component)
) WITH (OIDS = false)
;
create index on module_component_values(component);
create index on module_component_values(module);
create index on module_component_values(node);
create index on module_component_values(node, component);

alter table module_component_values OWNER TO core;
