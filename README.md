# glisser

A Haskell game server for playing a Chickapig-inspired board game.

## Server Protocol

The server allows users to connect via TCP and communicate via the specified protocol. The server holds all connected users, and contains many games. Each game may have either 2 players (Teams A and D) or 4 players (Teams A, B, C, and D). The protocol that the server communicates to the client is as follows:

```
SET_BOARD  <board> # Set the client's board to the board FEN given.
SET_TEAM   <team>  # Set the client's team to A, B, C, or D. Influenced by the user's REQUEST_TEAM, but not guaranteed if two users requested the same team.
TURN_START <time>  # Signal to the client that their turn has begun, and they should MOVE before the given time runs out. If time is negative, then the client may take as long as it wishes.
GAME_END   <team>  # Signal to the client that the game has ended. Passes in the team that won.
RESPONSE   <code>  # Signal a response to the client's actions. Every command passed to the server will be met with a response.
MAKE_MOVE  <move>  # Signal to the client that a move was made by another user. The move has been validated.
```

### Board Specification

### Team Specification

### Response Code Specification

```
1: Success
10: Move Failed
11: 
```

## Client Protocol

A client is not and should not be unique to this repository, as the implementation here is solely for completeness and an actual implementation would involve some web interface or other UI. However, all clients must understand the server protocol and be able to pass back the client protocol. The protocol that the client communicates back to the server is as follows:

```
REQUEST_TEAM <team> # Request a specific team. If multiple clients request the same team, the server picks a random user to get the requested team and the other clients are randomly assigned a new team with lowest priority.
PING                # Server will return a `SUCCESS` once it receives this message. Can be used to calculate network latency.
SEND_MOVE    <move> # Send a move to the server (to be approved)
FORFEIT             # Forfeit the game
```



BOARD <FEN>
MOVE <move_notation> 
TURN_START
ERROR <error_code>
TURN_SUCCESS
GAME_END <winner>

List of error codes:
0: Move failed

OUTGOING:
CONNECT
DISCONNECT
JOIN_GAME <game_id>
LEAVE_GAME
MOVE <move_notation> 
FORFEIT

