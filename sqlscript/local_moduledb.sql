create table local_module_component_values (
  module text not null,
  component text not null,
  data bytea not null,
  PRIMARY KEY (module, component)
) WITH (OIDS = false)
;
create index on local_module_component_values(component);
create index on local_module_component_values(module);

alter table local_module_component_values OWNER TO core;
