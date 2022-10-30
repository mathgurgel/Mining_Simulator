# Underground Mining Simulator
We all know that underground mining is a dangerous activity. However, the increasing use of technology allows us to reduce risks using robots that, in this case, can perform tasks such as digging and collecting.  
With that said, this project is about **creating a program that simulates the execution of a robot in a mine**.

## How does it work
### Mine description language
The following table describes how a mine is computed

| Element            | Symbol     |
| ------------------ | ---------- |
| Empty space        | ` `        | 
| Mine entrance      | `E`        | 
| Wall               | `%`        |
| Dirt               | `.`        |
| Stone              | `*`        |
| 50 of material     | `?`        |
| 100 of material    | `:`        |
| 150 of material    | `;`        |
| Other quantities   | `$`        |

### Example

```
%%%%%%%%%%%%%%%
%***..........%
%***... ...*..%
%***... ..***.%
%.?.... ...*..%
%..     .. ...%
%.... .... ...%
%.:.. .... ...%
%.. .       ..%
%..*. .. .....%
%.... .. .;;..%
%.*.. ...;;..*%
%............$%
%.........   .%
%%%%%%%%%%%%%E%
```

### Robot commands language
| Instruction             | Symbol     | Cost (energy points) |
| ----------------------- | ---------- | -------------------- |
| Move robot to the left  | `L`        | `-1`                 |
| Move robot to the right | `R`        | `-1`                 |
| Move robot upward       | `U`        | `-1`                 |
| Move robot downward     | `D`        | `-1`                 |
| Collect material        | `C`        | `-10`                |
| Stay                    | `S`        | `+1`                 |

### Program execution
The program is executed by reading a **file** that contains a mine description (`.ldm`) and another one with a set of instructions for the robot (`.lcr`). If all the instructions can be done, the mine is updated and showed on screen. Otherwise, an error is outputed.

### Thanks for visiting us!

## Authors
- Luisa Calegari: [@lsclgr](https://github.com/lsclgr)
- Matheus Gurgel: [@mathgurgel](https://github.com/mathgurgel)
- Thiago Cecote: [@Cecote](https://github.com/Cecote)
