{-
   Vikunja API

   # Pagination Every endpoint capable of pagination will return two headers: * `x-pagination-total-pages`: The total number of available pages for this request * `x-pagination-result-count`: The number of items returned for this request. # Rights All endpoints which return a single item (project, task, etc.) - no array - will also return a `x-max-right` header with the max right the user has on this item as an int where `0` is `Read Only`, `1` is `Read & Write` and `2` is `Admin`. This can be used to show or hide ui elements based on the rights the user has. # Errors All errors have an error code and a human-readable error message in addition to the http status code. You should always check for the status code in the response, not only the http status code. Due to limitations in the swagger library we're using for this document, only one error per http status code is documented here. Make sure to check the [error docs](https://vikunja.io/docs/errors/) in Vikunja's documentation for a full list of available error codes. # Authorization **JWT-Auth:** Main authorization method, used for most of the requests. Needs `Authorization: Bearer <jwt-token>`-header to authenticate successfully.  **API Token:** You can create scoped API tokens for your user and use the token to make authenticated requests in the context of that user. The token must be provided via an `Authorization: Bearer <token>` header, similar to jwt auth. See the documentation for the `api` group to manage token creation and revocation.  **BasicAuth:** Only used when requesting tasks via CalDAV. <!-- ReDoc-Inject: <security-definitions> -->

   OpenAPI Version: 3.0.1
   Vikunja API API version: 0.24.6
   Contact: hello@vikunja.io
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Vikunja.API.Task
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Vikunja.API.Task where

import Vikunja.Core
import Vikunja.MimeTypes
import Vikunja.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Task

-- *** kindIdReactionsDeletePost

-- | @POST \/{kind}\/{id}\/reactions\/delete@
-- 
-- Removes the user's reaction
-- 
-- Removes the reaction of that user on that entity.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
kindIdReactionsDeletePost
  :: (Consumes KindIdReactionsDeletePost MimeJSON, MimeRender MimeJSON ModelsReaction)
  => ModelsReaction -- ^ "project" -  The reaction you want to add to the entity.
  -> Id -- ^ "id" -  Entity ID
  -> Kind -- ^ "kind" -  The kind of the entity. Can be either `tasks` or `comments` for task comments
  -> VikunjaRequest KindIdReactionsDeletePost MimeJSON ModelsMessage MimeJSON
kindIdReactionsDeletePost project (Id id) (Kind kind) =
  _mkRequest "POST" ["/",toPath kind,"/",toPath id,"/reactions/delete"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` project

data KindIdReactionsDeletePost 

-- | /Body Param/ "project" - The reaction you want to add to the entity.
instance HasBodyParam KindIdReactionsDeletePost ModelsReaction 

-- | @application/json@
instance Consumes KindIdReactionsDeletePost MimeJSON

-- | @application/json@
instance Produces KindIdReactionsDeletePost MimeJSON


-- *** kindIdReactionsGet

-- | @GET \/{kind}\/{id}\/reactions@
-- 
-- Get all reactions for an entity
-- 
-- Returns all reactions for an entity
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
kindIdReactionsGet
  :: Id -- ^ "id" -  Entity ID
  -> Kind -- ^ "kind" -  The kind of the entity. Can be either `tasks` or `comments` for task comments
  -> VikunjaRequest KindIdReactionsGet MimeNoContent [Map.Map String [UserUser]] MimeJSON
kindIdReactionsGet (Id id) (Kind kind) =
  _mkRequest "GET" ["/",toPath kind,"/",toPath id,"/reactions"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data KindIdReactionsGet  
-- | @application/json@
instance Produces KindIdReactionsGet MimeJSON


-- *** kindIdReactionsPut

-- | @PUT \/{kind}\/{id}\/reactions@
-- 
-- Add a reaction to an entity
-- 
-- Add a reaction to an entity. Will do nothing if the reaction already exists.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
kindIdReactionsPut
  :: (Consumes KindIdReactionsPut MimeJSON, MimeRender MimeJSON ModelsReaction)
  => ModelsReaction -- ^ "project" -  The reaction you want to add to the entity.
  -> Id -- ^ "id" -  Entity ID
  -> Kind -- ^ "kind" -  The kind of the entity. Can be either `tasks` or `comments` for task comments
  -> VikunjaRequest KindIdReactionsPut MimeJSON ModelsReaction MimeJSON
kindIdReactionsPut project (Id id) (Kind kind) =
  _mkRequest "PUT" ["/",toPath kind,"/",toPath id,"/reactions"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` project

data KindIdReactionsPut 

-- | /Body Param/ "project" - The reaction you want to add to the entity.
instance HasBodyParam KindIdReactionsPut ModelsReaction 

-- | @application/json@
instance Consumes KindIdReactionsPut MimeJSON

-- | @application/json@
instance Produces KindIdReactionsPut MimeJSON


-- *** projectsIdTasksPut

-- | @PUT \/projects\/{id}\/tasks@
-- 
-- Create a task
-- 
-- Inserts a task into a project.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdTasksPut
  :: (Consumes ProjectsIdTasksPut MimeJSON, MimeRender MimeJSON ModelsTask)
  => ModelsTask -- ^ "task" -  The task object
  -> Id -- ^ "id" -  Project ID
  -> VikunjaRequest ProjectsIdTasksPut MimeJSON ModelsTask MimeJSON
projectsIdTasksPut task (Id id) =
  _mkRequest "PUT" ["/projects/",toPath id,"/tasks"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` task

data ProjectsIdTasksPut 

-- | /Body Param/ "task" - The task object
instance HasBodyParam ProjectsIdTasksPut ModelsTask 

-- | @application/json@
instance Consumes ProjectsIdTasksPut MimeJSON

-- | @application/json@
instance Produces ProjectsIdTasksPut MimeJSON


-- *** projectsIdViewsViewTasksGet

-- | @GET \/projects\/{id}\/views\/{view}\/tasks@
-- 
-- Get tasks in a project
-- 
-- Returns all tasks for the current project.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdViewsViewTasksGet
  :: Id -- ^ "id" -  The project ID.
  -> View -- ^ "view" -  The project view ID.
  -> VikunjaRequest ProjectsIdViewsViewTasksGet MimeNoContent [ModelsTask] MimeJSON
projectsIdViewsViewTasksGet (Id id) (View view) =
  _mkRequest "GET" ["/projects/",toPath id,"/views/",toPath view,"/tasks"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsIdViewsViewTasksGet  

-- | /Optional Param/ "page" - The page number. Used for pagination. If not provided, the first page of results is returned.
instance HasOptionalParam ProjectsIdViewsViewTasksGet Page where
  applyOptionalParam req (Page xs) =
    req `addQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "per_page" - The maximum number of items per page. Note this parameter is limited by the configured maximum of items per page.
instance HasOptionalParam ProjectsIdViewsViewTasksGet PerPage where
  applyOptionalParam req (PerPage xs) =
    req `addQuery` toQuery ("per_page", Just xs)

-- | /Optional Param/ "s" - Search tasks by task text.
instance HasOptionalParam ProjectsIdViewsViewTasksGet S where
  applyOptionalParam req (S xs) =
    req `addQuery` toQuery ("s", Just xs)

-- | /Optional Param/ "sort_by" - The sorting parameter. You can pass this multiple times to get the tasks ordered by multiple different parametes, along with `order_by`. Possible values to sort by are `id`, `title`, `description`, `done`, `done_at`, `due_date`, `created_by_id`, `project_id`, `repeat_after`, `priority`, `start_date`, `end_date`, `hex_color`, `percent_done`, `uid`, `created`, `updated`. Default is `id`.
instance HasOptionalParam ProjectsIdViewsViewTasksGet SortBy where
  applyOptionalParam req (SortBy xs) =
    req `addQuery` toQuery ("sort_by", Just xs)

-- | /Optional Param/ "order_by" - The ordering parameter. Possible values to order by are `asc` or `desc`. Default is `asc`.
instance HasOptionalParam ProjectsIdViewsViewTasksGet OrderBy where
  applyOptionalParam req (OrderBy xs) =
    req `addQuery` toQuery ("order_by", Just xs)

-- | /Optional Param/ "filter" - The filter query to match tasks by. Check out https://vikunja.io/docs/filters for a full explanation of the feature.
instance HasOptionalParam ProjectsIdViewsViewTasksGet Filter where
  applyOptionalParam req (Filter xs) =
    req `addQuery` toQuery ("filter", Just xs)

-- | /Optional Param/ "filter_timezone" - The time zone which should be used for date match (statements like 
instance HasOptionalParam ProjectsIdViewsViewTasksGet FilterTimezone where
  applyOptionalParam req (FilterTimezone xs) =
    req `addQuery` toQuery ("filter_timezone", Just xs)

-- | /Optional Param/ "filter_include_nulls" - If set to true the result will include filtered fields whose value is set to `null`. Available values are `true` or `false`. Defaults to `false`.
instance HasOptionalParam ProjectsIdViewsViewTasksGet FilterIncludeNulls where
  applyOptionalParam req (FilterIncludeNulls xs) =
    req `addQuery` toQuery ("filter_include_nulls", Just xs)

-- | /Optional Param/ "expand" - If set to `subtasks`, Vikunja will fetch only tasks which do not have subtasks and then in a second step, will fetch all of these subtasks. This may result in more tasks than the pagination limit being returned, but all subtasks will be present in the response. You can only set this to `subtasks`.
instance HasOptionalParam ProjectsIdViewsViewTasksGet Expand where
  applyOptionalParam req (Expand xs) =
    req `addQuery` toQuery ("expand", Just xs)
-- | @application/json@
instance Produces ProjectsIdViewsViewTasksGet MimeJSON


-- *** projectsProjectViewsViewBucketsBucketTasksPost

-- | @POST \/projects\/{project}\/views\/{view}\/buckets\/{bucket}\/tasks@
-- 
-- Update a task bucket
-- 
-- Updates a task in a bucket
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsProjectViewsViewBucketsBucketTasksPost
  :: (Consumes ProjectsProjectViewsViewBucketsBucketTasksPost MimeJSON, MimeRender MimeJSON ModelsTaskBucket)
  => ModelsTaskBucket -- ^ "taskBucket" -  The id of the task you want to move into the bucket.
  -> Project -- ^ "project" -  Project ID
  -> View -- ^ "view" -  Project View ID
  -> Bucket -- ^ "bucket" -  Bucket ID
  -> VikunjaRequest ProjectsProjectViewsViewBucketsBucketTasksPost MimeJSON ModelsTaskBucket MimeJSON
projectsProjectViewsViewBucketsBucketTasksPost taskBucket (Project project) (View view) (Bucket bucket) =
  _mkRequest "POST" ["/projects/",toPath project,"/views/",toPath view,"/buckets/",toPath bucket,"/tasks"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` taskBucket

data ProjectsProjectViewsViewBucketsBucketTasksPost 

-- | /Body Param/ "taskBucket" - The id of the task you want to move into the bucket.
instance HasBodyParam ProjectsProjectViewsViewBucketsBucketTasksPost ModelsTaskBucket 

-- | @application/json@
instance Consumes ProjectsProjectViewsViewBucketsBucketTasksPost MimeJSON

-- | @application/json@
instance Produces ProjectsProjectViewsViewBucketsBucketTasksPost MimeJSON


-- *** tasksAllGet

-- | @GET \/tasks\/all@
-- 
-- Get tasks
-- 
-- Returns all tasks on any project the user has access to.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksAllGet
  :: VikunjaRequest TasksAllGet MimeNoContent [ModelsTask] MimeJSON
tasksAllGet =
  _mkRequest "GET" ["/tasks/all"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data TasksAllGet  

-- | /Optional Param/ "page" - The page number. Used for pagination. If not provided, the first page of results is returned.
instance HasOptionalParam TasksAllGet Page where
  applyOptionalParam req (Page xs) =
    req `addQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "per_page" - The maximum number of items per page. Note this parameter is limited by the configured maximum of items per page.
instance HasOptionalParam TasksAllGet PerPage where
  applyOptionalParam req (PerPage xs) =
    req `addQuery` toQuery ("per_page", Just xs)

-- | /Optional Param/ "s" - Search tasks by task text.
instance HasOptionalParam TasksAllGet S where
  applyOptionalParam req (S xs) =
    req `addQuery` toQuery ("s", Just xs)

-- | /Optional Param/ "sort_by" - The sorting parameter. You can pass this multiple times to get the tasks ordered by multiple different parametes, along with `order_by`. Possible values to sort by are `id`, `title`, `description`, `done`, `done_at`, `due_date`, `created_by_id`, `project_id`, `repeat_after`, `priority`, `start_date`, `end_date`, `hex_color`, `percent_done`, `uid`, `created`, `updated`. Default is `id`.
instance HasOptionalParam TasksAllGet SortBy where
  applyOptionalParam req (SortBy xs) =
    req `addQuery` toQuery ("sort_by", Just xs)

-- | /Optional Param/ "order_by" - The ordering parameter. Possible values to order by are `asc` or `desc`. Default is `asc`.
instance HasOptionalParam TasksAllGet OrderBy where
  applyOptionalParam req (OrderBy xs) =
    req `addQuery` toQuery ("order_by", Just xs)

-- | /Optional Param/ "filter" - The filter query to match tasks by. Check out https://vikunja.io/docs/filters for a full explanation of the feature.
instance HasOptionalParam TasksAllGet Filter where
  applyOptionalParam req (Filter xs) =
    req `addQuery` toQuery ("filter", Just xs)

-- | /Optional Param/ "filter_timezone" - The time zone which should be used for date match (statements like 
instance HasOptionalParam TasksAllGet FilterTimezone where
  applyOptionalParam req (FilterTimezone xs) =
    req `addQuery` toQuery ("filter_timezone", Just xs)

-- | /Optional Param/ "filter_include_nulls" - If set to true the result will include filtered fields whose value is set to `null`. Available values are `true` or `false`. Defaults to `false`.
instance HasOptionalParam TasksAllGet FilterIncludeNulls where
  applyOptionalParam req (FilterIncludeNulls xs) =
    req `addQuery` toQuery ("filter_include_nulls", Just xs)

-- | /Optional Param/ "expand" - If set to `subtasks`, Vikunja will fetch only tasks which do not have subtasks and then in a second step, will fetch all of these subtasks. This may result in more tasks than the pagination limit being returned, but all subtasks will be present in the response. You can only set this to `subtasks`.
instance HasOptionalParam TasksAllGet Expand where
  applyOptionalParam req (Expand xs) =
    req `addQuery` toQuery ("expand", Just xs)
-- | @application/json@
instance Produces TasksAllGet MimeJSON


-- *** tasksBulkPost

-- | @POST \/tasks\/bulk@
-- 
-- Update a bunch of tasks at once
-- 
-- Updates a bunch of tasks at once. This includes marking them as done. Note: although you could supply another ID, it will be ignored. Use task_ids instead.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksBulkPost
  :: (Consumes TasksBulkPost MimeJSON, MimeRender MimeJSON ModelsBulkTask)
  => ModelsBulkTask -- ^ "task" -  The task object. Looks like a normal task, the only difference is it uses an array of project_ids to update.
  -> VikunjaRequest TasksBulkPost MimeJSON ModelsTask MimeJSON
tasksBulkPost task =
  _mkRequest "POST" ["/tasks/bulk"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` task

data TasksBulkPost 

-- | /Body Param/ "task" - The task object. Looks like a normal task, the only difference is it uses an array of project_ids to update.
instance HasBodyParam TasksBulkPost ModelsBulkTask 

-- | @application/json@
instance Consumes TasksBulkPost MimeJSON

-- | @application/json@
instance Produces TasksBulkPost MimeJSON


-- *** tasksIdAttachmentsAttachmentIDDelete

-- | @DELETE \/tasks\/{id}\/attachments\/{attachmentID}@
-- 
-- Delete an attachment
-- 
-- Delete an attachment.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksIdAttachmentsAttachmentIDDelete
  :: Id -- ^ "id" -  Task ID
  -> AttachmentId -- ^ "attachmentId" -  Attachment ID
  -> VikunjaRequest TasksIdAttachmentsAttachmentIDDelete MimeNoContent ModelsMessage MimeJSON
tasksIdAttachmentsAttachmentIDDelete (Id id) (AttachmentId attachmentId) =
  _mkRequest "DELETE" ["/tasks/",toPath id,"/attachments/",toPath attachmentId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data TasksIdAttachmentsAttachmentIDDelete  
-- | @application/json@
instance Produces TasksIdAttachmentsAttachmentIDDelete MimeJSON


-- *** tasksIdAttachmentsAttachmentIDGet

-- | @GET \/tasks\/{id}\/attachments\/{attachmentID}@
-- 
-- Get one attachment.
-- 
-- Get one attachment for download. **Returns json on error.**
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksIdAttachmentsAttachmentIDGet
  :: Id -- ^ "id" -  Task ID
  -> AttachmentId -- ^ "attachmentId" -  Attachment ID
  -> VikunjaRequest TasksIdAttachmentsAttachmentIDGet MimeNoContent FilePath MimeOctetStream
tasksIdAttachmentsAttachmentIDGet (Id id) (AttachmentId attachmentId) =
  _mkRequest "GET" ["/tasks/",toPath id,"/attachments/",toPath attachmentId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data TasksIdAttachmentsAttachmentIDGet  

-- | /Optional Param/ "preview_size" - The size of the preview image. Can be sm = 100px, md = 200px, lg = 400px or xl = 800px. If provided, a preview image will be returned if the attachment is an image.
instance HasOptionalParam TasksIdAttachmentsAttachmentIDGet PreviewSize where
  applyOptionalParam req (PreviewSize xs) =
    req `addQuery` toQuery ("preview_size", Just xs)
-- | @application/octet-stream@
instance Produces TasksIdAttachmentsAttachmentIDGet MimeOctetStream


-- *** tasksIdAttachmentsGet

-- | @GET \/tasks\/{id}\/attachments@
-- 
-- Get  all attachments for one task.
-- 
-- Get all task attachments for one task.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksIdAttachmentsGet
  :: Id -- ^ "id" -  Task ID
  -> VikunjaRequest TasksIdAttachmentsGet MimeNoContent [ModelsTaskAttachment] MimeJSON
tasksIdAttachmentsGet (Id id) =
  _mkRequest "GET" ["/tasks/",toPath id,"/attachments"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data TasksIdAttachmentsGet  

-- | /Optional Param/ "page" - The page number. Used for pagination. If not provided, the first page of results is returned.
instance HasOptionalParam TasksIdAttachmentsGet Page where
  applyOptionalParam req (Page xs) =
    req `addQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "per_page" - The maximum number of items per page. Note this parameter is limited by the configured maximum of items per page.
instance HasOptionalParam TasksIdAttachmentsGet PerPage where
  applyOptionalParam req (PerPage xs) =
    req `addQuery` toQuery ("per_page", Just xs)
-- | @application/json@
instance Produces TasksIdAttachmentsGet MimeJSON


-- *** tasksIdAttachmentsPut

-- | @PUT \/tasks\/{id}\/attachments@
-- 
-- Upload a task attachment
-- 
-- Upload a task attachment. You can pass multiple files with the files form param.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksIdAttachmentsPut
  :: (Consumes TasksIdAttachmentsPut MimeMultipartFormData)
  => Files -- ^ "files" -  The file, as multipart form file. You can pass multiple.
  -> Id -- ^ "id" -  Task ID
  -> VikunjaRequest TasksIdAttachmentsPut MimeMultipartFormData ModelsMessage MimeJSON
tasksIdAttachmentsPut (Files files) (Id id) =
  _mkRequest "PUT" ["/tasks/",toPath id,"/attachments"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `_addMultiFormPart` NH.partLBS "files" (mimeRender' MimeMultipartFormData files)

data TasksIdAttachmentsPut  

-- | @multipart/form-data@
instance Consumes TasksIdAttachmentsPut MimeMultipartFormData

-- | @application/json@
instance Produces TasksIdAttachmentsPut MimeJSON


-- *** tasksIdDelete

-- | @DELETE \/tasks\/{id}@
-- 
-- Delete a task
-- 
-- Deletes a task from a project. This does not mean \"mark it done\".
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksIdDelete
  :: Id -- ^ "id" -  Task ID
  -> VikunjaRequest TasksIdDelete MimeNoContent ModelsMessage MimeJSON
tasksIdDelete (Id id) =
  _mkRequest "DELETE" ["/tasks/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data TasksIdDelete  
-- | @application/json@
instance Produces TasksIdDelete MimeJSON


-- *** tasksIdGet

-- | @GET \/tasks\/{id}@
-- 
-- Get one task
-- 
-- Returns one task by its ID
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksIdGet
  :: Id -- ^ "id" -  The task ID
  -> VikunjaRequest TasksIdGet MimeNoContent ModelsTask MimeJSON
tasksIdGet (Id id) =
  _mkRequest "GET" ["/tasks/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data TasksIdGet  
-- | @application/json@
instance Produces TasksIdGet MimeJSON


-- *** tasksIdPositionPost

-- | @POST \/tasks\/{id}\/position@
-- 
-- Updates a task position
-- 
-- Updates a task position.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksIdPositionPost
  :: (Consumes TasksIdPositionPost MimeJSON, MimeRender MimeJSON ModelsTaskPosition)
  => ModelsTaskPosition -- ^ "view" -  The task position with updated values you want to change.
  -> Id -- ^ "id" -  Task ID
  -> VikunjaRequest TasksIdPositionPost MimeJSON ModelsTaskPosition MimeJSON
tasksIdPositionPost view (Id id) =
  _mkRequest "POST" ["/tasks/",toPath id,"/position"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` view

data TasksIdPositionPost 

-- | /Body Param/ "view" - The task position with updated values you want to change.
instance HasBodyParam TasksIdPositionPost ModelsTaskPosition 

-- | @application/json@
instance Consumes TasksIdPositionPost MimeJSON

-- | @application/json@
instance Produces TasksIdPositionPost MimeJSON


-- *** tasksIdPost

-- | @POST \/tasks\/{id}@
-- 
-- Update a task
-- 
-- Updates a task. This includes marking it as done. Assignees you pass will be updated, see their individual endpoints for more details on how this is done. To update labels, see the description of the endpoint.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksIdPost
  :: (Consumes TasksIdPost MimeJSON, MimeRender MimeJSON ModelsTask)
  => ModelsTask -- ^ "task" -  The task object
  -> Id -- ^ "id" -  The Task ID
  -> VikunjaRequest TasksIdPost MimeJSON ModelsTask MimeJSON
tasksIdPost task (Id id) =
  _mkRequest "POST" ["/tasks/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` task

data TasksIdPost 

-- | /Body Param/ "task" - The task object
instance HasBodyParam TasksIdPost ModelsTask 

-- | @application/json@
instance Consumes TasksIdPost MimeJSON

-- | @application/json@
instance Produces TasksIdPost MimeJSON


-- *** tasksTaskIDCommentsCommentIDDelete

-- | @DELETE \/tasks\/{taskID}\/comments\/{commentID}@
-- 
-- Remove a task comment
-- 
-- Remove a task comment. The user doing this need to have at least write access to the task this comment belongs to.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksTaskIDCommentsCommentIDDelete
  :: TaskId -- ^ "taskId" -  Task ID
  -> CommentId -- ^ "commentId" -  Comment ID
  -> VikunjaRequest TasksTaskIDCommentsCommentIDDelete MimeNoContent ModelsMessage MimeJSON
tasksTaskIDCommentsCommentIDDelete (TaskId taskId) (CommentId commentId) =
  _mkRequest "DELETE" ["/tasks/",toPath taskId,"/comments/",toPath commentId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data TasksTaskIDCommentsCommentIDDelete  
-- | @application/json@
instance Produces TasksTaskIDCommentsCommentIDDelete MimeJSON


-- *** tasksTaskIDCommentsCommentIDGet

-- | @GET \/tasks\/{taskID}\/comments\/{commentID}@
-- 
-- Remove a task comment
-- 
-- Remove a task comment. The user doing this need to have at least read access to the task this comment belongs to.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksTaskIDCommentsCommentIDGet
  :: TaskId -- ^ "taskId" -  Task ID
  -> CommentId -- ^ "commentId" -  Comment ID
  -> VikunjaRequest TasksTaskIDCommentsCommentIDGet MimeNoContent ModelsTaskComment MimeJSON
tasksTaskIDCommentsCommentIDGet (TaskId taskId) (CommentId commentId) =
  _mkRequest "GET" ["/tasks/",toPath taskId,"/comments/",toPath commentId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data TasksTaskIDCommentsCommentIDGet  
-- | @application/json@
instance Produces TasksTaskIDCommentsCommentIDGet MimeJSON


-- *** tasksTaskIDCommentsCommentIDPost

-- | @POST \/tasks\/{taskID}\/comments\/{commentID}@
-- 
-- Update an existing task comment
-- 
-- Update an existing task comment. The user doing this need to have at least write access to the task this comment belongs to.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksTaskIDCommentsCommentIDPost
  :: TaskId -- ^ "taskId" -  Task ID
  -> CommentId -- ^ "commentId" -  Comment ID
  -> VikunjaRequest TasksTaskIDCommentsCommentIDPost MimeNoContent ModelsTaskComment MimeJSON
tasksTaskIDCommentsCommentIDPost (TaskId taskId) (CommentId commentId) =
  _mkRequest "POST" ["/tasks/",toPath taskId,"/comments/",toPath commentId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data TasksTaskIDCommentsCommentIDPost  
-- | @application/json@
instance Produces TasksTaskIDCommentsCommentIDPost MimeJSON


-- *** tasksTaskIDCommentsGet

-- | @GET \/tasks\/{taskID}\/comments@
-- 
-- Get all task comments
-- 
-- Get all task comments. The user doing this need to have at least read access to the task.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksTaskIDCommentsGet
  :: TaskId -- ^ "taskId" -  Task ID
  -> VikunjaRequest TasksTaskIDCommentsGet MimeNoContent [ModelsTaskComment] MimeJSON
tasksTaskIDCommentsGet (TaskId taskId) =
  _mkRequest "GET" ["/tasks/",toPath taskId,"/comments"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data TasksTaskIDCommentsGet  
-- | @application/json@
instance Produces TasksTaskIDCommentsGet MimeJSON


-- *** tasksTaskIDCommentsPut

-- | @PUT \/tasks\/{taskID}\/comments@
-- 
-- Create a new task comment
-- 
-- Create a new task comment. The user doing this need to have at least write access to the task this comment should belong to.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksTaskIDCommentsPut
  :: (Consumes TasksTaskIDCommentsPut MimeJSON, MimeRender MimeJSON ModelsTaskComment)
  => ModelsTaskComment -- ^ "relation" -  The task comment object
  -> TaskId -- ^ "taskId" -  Task ID
  -> VikunjaRequest TasksTaskIDCommentsPut MimeJSON ModelsTaskComment MimeJSON
tasksTaskIDCommentsPut relation (TaskId taskId) =
  _mkRequest "PUT" ["/tasks/",toPath taskId,"/comments"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` relation

data TasksTaskIDCommentsPut 

-- | /Body Param/ "relation" - The task comment object
instance HasBodyParam TasksTaskIDCommentsPut ModelsTaskComment 

-- | @application/json@
instance Consumes TasksTaskIDCommentsPut MimeJSON

-- | @application/json@
instance Produces TasksTaskIDCommentsPut MimeJSON


-- *** tasksTaskIDRelationsPut

-- | @PUT \/tasks\/{taskID}\/relations@
-- 
-- Create a new relation between two tasks
-- 
-- Creates a new relation between two tasks. The user needs to have update rights on the base task and at least read rights on the other task. Both tasks do not need to be on the same project. Take a look at the docs for available task relation kinds.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksTaskIDRelationsPut
  :: (Consumes TasksTaskIDRelationsPut MimeJSON, MimeRender MimeJSON ModelsTaskRelation)
  => ModelsTaskRelation -- ^ "relation" -  The relation object
  -> TaskId -- ^ "taskId" -  Task ID
  -> VikunjaRequest TasksTaskIDRelationsPut MimeJSON ModelsTaskRelation MimeJSON
tasksTaskIDRelationsPut relation (TaskId taskId) =
  _mkRequest "PUT" ["/tasks/",toPath taskId,"/relations"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` relation

data TasksTaskIDRelationsPut 

-- | /Body Param/ "relation" - The relation object
instance HasBodyParam TasksTaskIDRelationsPut ModelsTaskRelation 

-- | @application/json@
instance Consumes TasksTaskIDRelationsPut MimeJSON

-- | @application/json@
instance Produces TasksTaskIDRelationsPut MimeJSON


-- *** tasksTaskIDRelationsRelationKindOtherTaskIDDelete

-- | @DELETE \/tasks\/{taskID}\/relations\/{relationKind}\/{otherTaskID}@
-- 
-- Remove a task relation
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
tasksTaskIDRelationsRelationKindOtherTaskIDDelete
  :: (Consumes TasksTaskIDRelationsRelationKindOtherTaskIDDelete MimeJSON, MimeRender MimeJSON ModelsTaskRelation)
  => ModelsTaskRelation -- ^ "relation" -  The relation object
  -> TaskId -- ^ "taskId" -  Task ID
  -> RelationKind -- ^ "relationKind" -  The kind of the relation. See the TaskRelation type for more info.
  -> OtherTaskId -- ^ "otherTaskId" -  The id of the other task.
  -> VikunjaRequest TasksTaskIDRelationsRelationKindOtherTaskIDDelete MimeJSON ModelsMessage MimeJSON
tasksTaskIDRelationsRelationKindOtherTaskIDDelete relation (TaskId taskId) (RelationKind relationKind) (OtherTaskId otherTaskId) =
  _mkRequest "DELETE" ["/tasks/",toPath taskId,"/relations/",toPath relationKind,"/",toPath otherTaskId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` relation

data TasksTaskIDRelationsRelationKindOtherTaskIDDelete 

-- | /Body Param/ "relation" - The relation object
instance HasBodyParam TasksTaskIDRelationsRelationKindOtherTaskIDDelete ModelsTaskRelation 

-- | @application/json@
instance Consumes TasksTaskIDRelationsRelationKindOtherTaskIDDelete MimeJSON

-- | @application/json@
instance Produces TasksTaskIDRelationsRelationKindOtherTaskIDDelete MimeJSON

