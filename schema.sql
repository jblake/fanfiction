drop type site cascade;

drop table stories cascade;
drop table sources cascade;
drop table tags cascade;

drop view all_tags cascade;
drop view story_tags cascade;
drop view unpruned_story_tags cascade;

drop function add_story( ) cascade;
drop function del_story( int ) cascade;
drop function add_source( int, site, text ) cascade;
drop function add_tag( int, text ) cascade;
drop function del_tag( int, text ) cascade;

begin;

-- This type should be ordered such that better sources are earlier in the list.
create type site as enum
  ( 'fanfiction.net'
  , 'ficsite.com'
  , 'ficwad.com'
  , 'hpfanficarchive.com'
  , 'patronuscharm.net'
  , 'portkey.org'
  );

create table stories
  ( id serial not null
  , pruned boolean not null default 'f'
  , primary key ( id )
  );

create table sources
  ( story int not null references stories on delete cascade
  , source site not null
  , ref text not null
  , primary key ( story, source )
  );

create table tags
  ( story int not null references stories on delete cascade
  , tag text not null
  , primary key ( story, tag )
  );

create view all_tags as
  select tag, count(*) as uses
  from tags
  group by tag
  order by uses desc, tag;

create view story_tags as
  select tags.story, array_agg(tags.tag) as tags
  from tags
  group by tags.story;

create view unpruned_story_tags as
  select story_tags.*
  from story_tags
  inner join stories on (story_tags.story = stories.id)
  where not pruned;

create function add_story( ) returns int strict volatile as $$
  begin
    insert into stories default values;
    return currval( 'stories_id_seq' );
  end;
  $$ language plpgsql;

create function del_story( the_story int ) returns void strict volatile as $$
  begin
    update stories set pruned = 't' where id = the_story;
  end;
  $$ language plpgsql;

create function add_source( the_story int, the_source site, the_ref text ) returns void strict volatile as $$
  begin
    delete from sources where story = the_story and source = the_source;
    insert into sources ( story, source, ref ) values ( the_story, the_source, the_ref );
  end;
  $$ language plpgsql;

create function add_tag( the_story int, the_tag text ) returns void strict volatile as $$
  begin
    if not exists (select * from tags where story = the_story and tag = the_tag) then
      insert into tags ( story, tag ) values ( the_story, the_tag );
    end if;
  end;
  $$ language plpgsql;

create function del_tag( the_story int, the_tag text ) returns void strict volatile as $$
  begin
    delete from tags where story = the_story and tag = the_tag;
  end;
  $$ language plpgsql;

commit;
