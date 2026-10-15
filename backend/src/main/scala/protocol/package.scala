/*
 * Copyright (c) 2026 wayfarerx.net.
 *
 * This file is licensed to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.wayfarerx.wizlights
package backend

/**
 * Shared definitions in the JSON package.
 */
package object protocol:

  /** The name of the "error" key. */
  private val ErrorKey = "error"

  /** The name of the "method" key. */
  private val MethodKey = "method"

  /** The name of the "params" key. */
  private val ParamsKey = "params"

  /** The name of the "result" key. */
  private val ResultKey = "result"

  /** The name of the "firstBeat" method. */
  private val FirstBeatMethod = "firstBeat"
  
  /** The name of the "getPilot" method. */
  private val GetPilotMethod = "getPilot"

  /** The name of the "getSystemConfig" method. */
  private val GetSystemConfigMethod = "getSystemConfig"

  /** The name of the "registration" method. */
  private val RegistrationMethod = "registration"
  
  /** The name of the "setPilot" method. */
  private val SetPilotMethod = "setPilot"

  /** The name of the "syncPilot" method. */
  private val SyncPilotMethod = "syncPilot"
