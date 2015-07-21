Orbis UUID
==========

Orbis UUID is an OTP application for generating UUID's.

Usage
-----

1. Include `orbis_uuid` as a dependency for your project.

2. Set the application environment variables in your `sys.config` for
   (`orbis_uuid`, `epoch_offset`) and (`orbis_uuid`, `pool_size`).

3. Use `orbis_uuid:create/0` to generate UUID's.

Authors
-------

- [Alexander Færøy](https://twitter.com/ahfaeroey).

Licence
-------

    Copyright (c) 2015 Alexander Færøy

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
