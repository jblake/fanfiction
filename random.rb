#!/usr/bin/ruby -w

$num = ARGV.shift
if ( not $num ) then
  $num = 1
else
  $num = $num.to_i
end

IO.popen( 'ls -l books | awk \'{ print $5, $8 }\'', 'r' ) do | books |
  $lines = books.read.split( "\n" ).map { |l| /^(\d+) (.+)$/.match( l ) }.select { |m| m }.map { |m| [ m[1].to_i, m[2] ] }
end

$total = $lines.map { |i| i[0] }.reduce( :+ )

system( "ssh root@zulu.omgwallhack.org \"while pkill com.flyersoft.moonreaderp; do sleep 1; done\"" )

IO.popen( "ssh root@zulu.omgwallhack.org sqlite3 -batch /data/data/com.flyersoft.moonreaderp/databases/mrbooks.db", "w" ) do | sql |

  sql.write( "BEGIN;\n" )

  while ( $num > 0 and $total > 0 and $lines.length > 0 ) do

    $n = rand( $total )

    $i = 0

    while ( $n >= $lines[$i][0] ) do
      $n -= $lines[$i][0]
      $i += 1
    end

    info = $lines.delete_at( $i )

    $num -= 1
    $total -= info[0]

    $stdout.write( "#{info[0]}\t#{info[1]}\n" )

    sql.write( "UPDATE books SET favorite = 'default_fav' WHERE filename = '/sdcard/Books/#{info[1]}';\n" )

  end

  sql.write( "COMMIT;\n.quit\n" )

end
