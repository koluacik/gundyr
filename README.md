# gundyr

A Discord Bot that uses [calamity](https://github.com/simmsb/calamity).

The bot listens for new/deleted emoji reactions for assigning roles to users in a server.

A role can be configured to have relations between other roles such that the role can be assigned to a member only if the member (has / does not have) another role. Moreover a role can be configured to remove another role upon assignment. All of these can be configured with the bot command interface. (see: `!help` and `!help role`).

Furthermore, Gundyr can be instructed to send new messages and edit or delete them, and build relations between message-emoji-role triples so that an emoji reaction to a message triggers role assignment. (see: `!help reamoji` and `!help msg`)

Gundyr requires admin and manage server roles permissions for their !-commands.


## TODO:

- Bot presence
- Database migration
