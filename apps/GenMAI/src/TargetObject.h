//   Copyright 2017 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

#ifndef TARGETOBJECT_H
#define TARGETOBJECT_H

/**
 * @brief Mother class of targets for Builder objects
 */

class TargetObject
{
public:
    TargetObject();
    /// Prints a summary of the object to stdout
    virtual void print() const = 0;
    /// Lists the internal data of the object to stdout
    virtual void list() const = 0;
    /// Tells if the object is empty or not
    virtual bool isEmpty() const = 0;
    /// Deletes everything
    virtual void clear() = 0;
};

#include "TargetObject.inl"

#endif
