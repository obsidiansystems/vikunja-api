{-
   Vikunja API

   # Pagination Every endpoint capable of pagination will return two headers: * `x-pagination-total-pages`: The total number of available pages for this request * `x-pagination-result-count`: The number of items returned for this request. # Rights All endpoints which return a single item (project, task, etc.) - no array - will also return a `x-max-right` header with the max right the user has on this item as an int where `0` is `Read Only`, `1` is `Read & Write` and `2` is `Admin`. This can be used to show or hide ui elements based on the rights the user has. # Errors All errors have an error code and a human-readable error message in addition to the http status code. You should always check for the status code in the response, not only the http status code. Due to limitations in the swagger library we're using for this document, only one error per http status code is documented here. Make sure to check the [error docs](https://vikunja.io/docs/errors/) in Vikunja's documentation for a full list of available error codes. # Authorization **JWT-Auth:** Main authorization method, used for most of the requests. Needs `Authorization: Bearer <jwt-token>`-header to authenticate successfully.  **API Token:** You can create scoped API tokens for your user and use the token to make authenticated requests in the context of that user. The token must be provided via an `Authorization: Bearer <token>` header, similar to jwt auth. See the documentation for the `api` group to manage token creation and revocation.  **BasicAuth:** Only used when requesting tasks via CalDAV. <!-- ReDoc-Inject: <security-definitions> -->

   OpenAPI Version: 3.0.1
   Vikunja API API version: 0.24.6
   Contact: hello@vikunja.io
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Vikunja.API
-}

module Vikunja.API
  ( module Vikunja.API.Api
  , module Vikunja.API.Assignees
  , module Vikunja.API.Auth
  , module Vikunja.API.Filter
  , module Vikunja.API.Labels
  , module Vikunja.API.Migration
  , module Vikunja.API.Project
  , module Vikunja.API.Service
  , module Vikunja.API.Sharing
  , module Vikunja.API.Subscriptions
  , module Vikunja.API.Task
  , module Vikunja.API.Team
  , module Vikunja.API.Testing
  , module Vikunja.API.User
  , module Vikunja.API.Webhooks
  ) where

import Vikunja.API.Api
import Vikunja.API.Assignees
import Vikunja.API.Auth
import Vikunja.API.Filter
import Vikunja.API.Labels
import Vikunja.API.Migration
import Vikunja.API.Project
import Vikunja.API.Service
import Vikunja.API.Sharing
import Vikunja.API.Subscriptions
import Vikunja.API.Task
import Vikunja.API.Team
import Vikunja.API.Testing
import Vikunja.API.User
import Vikunja.API.Webhooks