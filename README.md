# Tieminos

My current music workshop, all my clojure code-music creation since mid-2021 lives here.

Mainly uses `Supercollider` via `Overtone`, my microtonality library [`erv`](https://github.com/diegovdc/erv) and my sequencing library [`time-tiem`](https://github.com/diegovdc/time-tiem).

## Usage

Different sections need different things, but to connect to Supercollider you will need to connect using the standard [`overtone`](https://github.com/overtone/overtone) procedure. The midi sections require `virmidi` (linux) and the following command must be run in a terminal window to enable it: `sudo modprobe snd-virmidi`

## License

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
