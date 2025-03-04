{-
   Vikunja API

   # Pagination Every endpoint capable of pagination will return two headers: * `x-pagination-total-pages`: The total number of available pages for this request * `x-pagination-result-count`: The number of items returned for this request. # Rights All endpoints which return a single item (project, task, etc.) - no array - will also return a `x-max-right` header with the max right the user has on this item as an int where `0` is `Read Only`, `1` is `Read & Write` and `2` is `Admin`. This can be used to show or hide ui elements based on the rights the user has. # Errors All errors have an error code and a human-readable error message in addition to the http status code. You should always check for the status code in the response, not only the http status code. Due to limitations in the swagger library we're using for this document, only one error per http status code is documented here. Make sure to check the [error docs](https://vikunja.io/docs/errors/) in Vikunja's documentation for a full list of available error codes. # Authorization **JWT-Auth:** Main authorization method, used for most of the requests. Needs `Authorization: Bearer <jwt-token>`-header to authenticate successfully.  **API Token:** You can create scoped API tokens for your user and use the token to make authenticated requests in the context of that user. The token must be provided via an `Authorization: Bearer <token>` header, similar to jwt auth. See the documentation for the `api` group to manage token creation and revocation.  **BasicAuth:** Only used when requesting tasks via CalDAV. <!-- ReDoc-Inject: <security-definitions> -->

   OpenAPI Version: 3.0.1
   Vikunja API API version: 0.24.6
   Contact: hello@vikunja.io
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Vikunja.API.Project
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Vikunja.API.Project where

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


-- ** Project

-- *** backgroundsUnsplashImageImageGet

-- | @GET \/backgrounds\/unsplash\/image\/{image}@
-- 
-- Get an unsplash image
-- 
-- Get an unsplash image. **Returns json on error.**
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
backgroundsUnsplashImageImageGet
  :: Image -- ^ "image" -  Unsplash Image ID
  -> VikunjaRequest BackgroundsUnsplashImageImageGet MimeNoContent FilePath MimeOctetStream
backgroundsUnsplashImageImageGet (Image image) =
  _mkRequest "GET" ["/backgrounds/unsplash/image/",toPath image]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data BackgroundsUnsplashImageImageGet  
-- | @application/octet-stream@
instance Produces BackgroundsUnsplashImageImageGet MimeOctetStream


-- *** backgroundsUnsplashImageImageThumbGet

-- | @GET \/backgrounds\/unsplash\/image\/{image}\/thumb@
-- 
-- Get an unsplash thumbnail image
-- 
-- Get an unsplash thumbnail image. The thumbnail is cropped to a max width of 200px. **Returns json on error.**
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
backgroundsUnsplashImageImageThumbGet
  :: Image -- ^ "image" -  Unsplash Image ID
  -> VikunjaRequest BackgroundsUnsplashImageImageThumbGet MimeNoContent FilePath MimeOctetStream
backgroundsUnsplashImageImageThumbGet (Image image) =
  _mkRequest "GET" ["/backgrounds/unsplash/image/",toPath image,"/thumb"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data BackgroundsUnsplashImageImageThumbGet  
-- | @application/octet-stream@
instance Produces BackgroundsUnsplashImageImageThumbGet MimeOctetStream


-- *** backgroundsUnsplashSearchGet

-- | @GET \/backgrounds\/unsplash\/search@
-- 
-- Search for a background from unsplash
-- 
-- Search for a project background from unsplash
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
backgroundsUnsplashSearchGet
  :: VikunjaRequest BackgroundsUnsplashSearchGet MimeNoContent [BackgroundImage] MimeJSON
backgroundsUnsplashSearchGet =
  _mkRequest "GET" ["/backgrounds/unsplash/search"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data BackgroundsUnsplashSearchGet  

-- | /Optional Param/ "s" - Search backgrounds from unsplash with this search term.
instance HasOptionalParam BackgroundsUnsplashSearchGet S where
  applyOptionalParam req (S xs) =
    req `addQuery` toQuery ("s", Just xs)

-- | /Optional Param/ "p" - The page number. Used for pagination. If not provided, the first page of results is returned.
instance HasOptionalParam BackgroundsUnsplashSearchGet P where
  applyOptionalParam req (P xs) =
    req `addQuery` toQuery ("p", Just xs)
-- | @application/json@
instance Produces BackgroundsUnsplashSearchGet MimeJSON


-- *** projectsGet

-- | @GET \/projects@
-- 
-- Get all projects a user has access to
-- 
-- Returns all projects a user has access to.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsGet
  :: VikunjaRequest ProjectsGet MimeNoContent [ModelsProject] MimeJSON
projectsGet =
  _mkRequest "GET" ["/projects"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsGet  

-- | /Optional Param/ "page" - The page number. Used for pagination. If not provided, the first page of results is returned.
instance HasOptionalParam ProjectsGet Page where
  applyOptionalParam req (Page xs) =
    req `addQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "per_page" - The maximum number of items per page. Note this parameter is limited by the configured maximum of items per page.
instance HasOptionalParam ProjectsGet PerPage where
  applyOptionalParam req (PerPage xs) =
    req `addQuery` toQuery ("per_page", Just xs)

-- | /Optional Param/ "s" - Search projects by title.
instance HasOptionalParam ProjectsGet S where
  applyOptionalParam req (S xs) =
    req `addQuery` toQuery ("s", Just xs)

-- | /Optional Param/ "is_archived" - If true, also returns all archived projects.
instance HasOptionalParam ProjectsGet IsArchived where
  applyOptionalParam req (IsArchived xs) =
    req `addQuery` toQuery ("is_archived", Just xs)
-- | @application/json@
instance Produces ProjectsGet MimeJSON


-- *** projectsIdBackgroundDelete

-- | @DELETE \/projects\/{id}\/background@
-- 
-- Remove a project background
-- 
-- Removes a previously set project background, regardless of the project provider used to set the background. It does not throw an error if the project does not have a background.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdBackgroundDelete
  :: Id -- ^ "id" -  Project ID
  -> VikunjaRequest ProjectsIdBackgroundDelete MimeNoContent ModelsProject MimeJSON
projectsIdBackgroundDelete (Id id) =
  _mkRequest "DELETE" ["/projects/",toPath id,"/background"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsIdBackgroundDelete  
-- | @application/json@
instance Produces ProjectsIdBackgroundDelete MimeJSON


-- *** projectsIdBackgroundGet

-- | @GET \/projects\/{id}\/background@
-- 
-- Get the project background
-- 
-- Get the project background of a specific project. **Returns json on error.**
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdBackgroundGet
  :: Id -- ^ "id" -  Project ID
  -> VikunjaRequest ProjectsIdBackgroundGet MimeNoContent FilePath MimeOctetStream
projectsIdBackgroundGet (Id id) =
  _mkRequest "GET" ["/projects/",toPath id,"/background"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsIdBackgroundGet  
-- | @application/octet-stream@
instance Produces ProjectsIdBackgroundGet MimeOctetStream


-- *** projectsIdBackgroundsUnsplashPost

-- | @POST \/projects\/{id}\/backgrounds\/unsplash@
-- 
-- Set an unsplash photo as project background
-- 
-- Sets a photo from unsplash as project background.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdBackgroundsUnsplashPost
  :: (Consumes ProjectsIdBackgroundsUnsplashPost MimeJSON, MimeRender MimeJSON BackgroundImage)
  => BackgroundImage -- ^ "project" -  The image you want to set as background
  -> Id -- ^ "id" -  Project ID
  -> VikunjaRequest ProjectsIdBackgroundsUnsplashPost MimeJSON ModelsProject MimeJSON
projectsIdBackgroundsUnsplashPost project (Id id) =
  _mkRequest "POST" ["/projects/",toPath id,"/backgrounds/unsplash"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` project

data ProjectsIdBackgroundsUnsplashPost 

-- | /Body Param/ "project" - The image you want to set as background
instance HasBodyParam ProjectsIdBackgroundsUnsplashPost BackgroundImage 

-- | @application/json@
instance Consumes ProjectsIdBackgroundsUnsplashPost MimeJSON

-- | @application/json@
instance Produces ProjectsIdBackgroundsUnsplashPost MimeJSON


-- *** projectsIdBackgroundsUploadPut

-- | @PUT \/projects\/{id}\/backgrounds\/upload@
-- 
-- Upload a project background
-- 
-- Upload a project background.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdBackgroundsUploadPut
  :: (Consumes ProjectsIdBackgroundsUploadPut MimeMultipartFormData)
  => Background -- ^ "background" -  The file as single file.
  -> Id -- ^ "id" -  Project ID
  -> VikunjaRequest ProjectsIdBackgroundsUploadPut MimeMultipartFormData ModelsMessage MimeJSON
projectsIdBackgroundsUploadPut (Background background) (Id id) =
  _mkRequest "PUT" ["/projects/",toPath id,"/backgrounds/upload"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `_addMultiFormPart` NH.partLBS "background" (mimeRender' MimeMultipartFormData background)

data ProjectsIdBackgroundsUploadPut  

-- | @multipart/form-data@
instance Consumes ProjectsIdBackgroundsUploadPut MimeMultipartFormData

-- | @application/json@
instance Produces ProjectsIdBackgroundsUploadPut MimeJSON


-- *** projectsIdDelete

-- | @DELETE \/projects\/{id}@
-- 
-- Deletes a project
-- 
-- Delets a project
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdDelete
  :: Id -- ^ "id" -  Project ID
  -> VikunjaRequest ProjectsIdDelete MimeNoContent ModelsMessage MimeJSON
projectsIdDelete (Id id) =
  _mkRequest "DELETE" ["/projects/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsIdDelete  
-- | @application/json@
instance Produces ProjectsIdDelete MimeJSON


-- *** projectsIdGet

-- | @GET \/projects\/{id}@
-- 
-- Gets one project
-- 
-- Returns a project by its ID.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdGet
  :: Id -- ^ "id" -  Project ID
  -> VikunjaRequest ProjectsIdGet MimeNoContent ModelsProject MimeJSON
projectsIdGet (Id id) =
  _mkRequest "GET" ["/projects/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsIdGet  
-- | @application/json@
instance Produces ProjectsIdGet MimeJSON


-- *** projectsIdPost

-- | @POST \/projects\/{id}@
-- 
-- Updates a project
-- 
-- Updates a project. This does not include adding a task (see below).
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdPost
  :: (Consumes ProjectsIdPost MimeJSON, MimeRender MimeJSON ModelsProject)
  => ModelsProject -- ^ "project" -  The project with updated values you want to update.
  -> Id -- ^ "id" -  Project ID
  -> VikunjaRequest ProjectsIdPost MimeJSON ModelsProject MimeJSON
projectsIdPost project (Id id) =
  _mkRequest "POST" ["/projects/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` project

data ProjectsIdPost 

-- | /Body Param/ "project" - The project with updated values you want to update.
instance HasBodyParam ProjectsIdPost ModelsProject 

-- | @application/json@
instance Consumes ProjectsIdPost MimeJSON

-- | @application/json@
instance Produces ProjectsIdPost MimeJSON


-- *** projectsIdProjectusersGet

-- | @GET \/projects\/{id}\/projectusers@
-- 
-- Get users
-- 
-- Lists all users (without emailadresses). Also possible to search for a specific user.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdProjectusersGet
  :: Id -- ^ "id" -  Project ID
  -> VikunjaRequest ProjectsIdProjectusersGet MimeNoContent [UserUser] MimeJSON
projectsIdProjectusersGet (Id id) =
  _mkRequest "GET" ["/projects/",toPath id,"/projectusers"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsIdProjectusersGet  

-- | /Optional Param/ "s" - Search for a user by its name.
instance HasOptionalParam ProjectsIdProjectusersGet S where
  applyOptionalParam req (S xs) =
    req `addQuery` toQuery ("s", Just xs)
-- | @application/json@
instance Produces ProjectsIdProjectusersGet MimeJSON


-- *** projectsIdViewsViewBucketsGet

-- | @GET \/projects\/{id}\/views\/{view}\/buckets@
-- 
-- Get all kanban buckets of a project
-- 
-- Returns all kanban buckets which belong to that project. Buckets are always sorted by their `position` in ascending order. To get all buckets with their tasks, use the tasks endpoint with a kanban view.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdViewsViewBucketsGet
  :: Id -- ^ "id" -  Project ID
  -> View -- ^ "view" -  Project view ID
  -> VikunjaRequest ProjectsIdViewsViewBucketsGet MimeNoContent [ModelsBucket] MimeJSON
projectsIdViewsViewBucketsGet (Id id) (View view) =
  _mkRequest "GET" ["/projects/",toPath id,"/views/",toPath view,"/buckets"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsIdViewsViewBucketsGet  
-- | @application/json@
instance Produces ProjectsIdViewsViewBucketsGet MimeJSON


-- *** projectsIdViewsViewBucketsPut

-- | @PUT \/projects\/{id}\/views\/{view}\/buckets@
-- 
-- Create a new bucket
-- 
-- Creates a new kanban bucket on a project.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsIdViewsViewBucketsPut
  :: (Consumes ProjectsIdViewsViewBucketsPut MimeJSON, MimeRender MimeJSON ModelsBucket)
  => ModelsBucket -- ^ "bucket" -  The bucket object
  -> Id -- ^ "id" -  Project Id
  -> View -- ^ "view" -  Project view ID
  -> VikunjaRequest ProjectsIdViewsViewBucketsPut MimeJSON ModelsBucket MimeJSON
projectsIdViewsViewBucketsPut bucket (Id id) (View view) =
  _mkRequest "PUT" ["/projects/",toPath id,"/views/",toPath view,"/buckets"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` bucket

data ProjectsIdViewsViewBucketsPut 

-- | /Body Param/ "bucket" - The bucket object
instance HasBodyParam ProjectsIdViewsViewBucketsPut ModelsBucket 

-- | @application/json@
instance Consumes ProjectsIdViewsViewBucketsPut MimeJSON

-- | @application/json@
instance Produces ProjectsIdViewsViewBucketsPut MimeJSON


-- *** projectsProjectIDDuplicatePut

-- | @PUT \/projects\/{projectID}\/duplicate@
-- 
-- Duplicate an existing project
-- 
-- Copies the project, tasks, files, kanban data, assignees, comments, attachments, lables, relations, backgrounds, user/team rights and link shares from one project to a new one. The user needs read access in the project and write access in the parent of the new project.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsProjectIDDuplicatePut
  :: (Consumes ProjectsProjectIDDuplicatePut MimeJSON, MimeRender MimeJSON ModelsProjectDuplicate)
  => ModelsProjectDuplicate -- ^ "project" -  The target parent project which should hold the copied project.
  -> ProjectId -- ^ "projectId" -  The project ID to duplicate
  -> VikunjaRequest ProjectsProjectIDDuplicatePut MimeJSON ModelsProjectDuplicate MimeJSON
projectsProjectIDDuplicatePut project (ProjectId projectId) =
  _mkRequest "PUT" ["/projects/",toPath projectId,"/duplicate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` project

data ProjectsProjectIDDuplicatePut 

-- | /Body Param/ "project" - The target parent project which should hold the copied project.
instance HasBodyParam ProjectsProjectIDDuplicatePut ModelsProjectDuplicate 

-- | @application/json@
instance Consumes ProjectsProjectIDDuplicatePut MimeJSON

-- | @application/json@
instance Produces ProjectsProjectIDDuplicatePut MimeJSON


-- *** projectsProjectIDViewsViewBucketsBucketIDDelete

-- | @DELETE \/projects\/{projectID}\/views\/{view}\/buckets\/{bucketID}@
-- 
-- Deletes an existing bucket
-- 
-- Deletes an existing kanban bucket and dissociates all of its task. It does not delete any tasks. You cannot delete the last bucket on a project.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsProjectIDViewsViewBucketsBucketIDDelete
  :: ProjectId -- ^ "projectId" -  Project Id
  -> BucketId -- ^ "bucketId" -  Bucket Id
  -> View -- ^ "view" -  Project view ID
  -> VikunjaRequest ProjectsProjectIDViewsViewBucketsBucketIDDelete MimeNoContent ModelsMessage MimeJSON
projectsProjectIDViewsViewBucketsBucketIDDelete (ProjectId projectId) (BucketId bucketId) (View view) =
  _mkRequest "DELETE" ["/projects/",toPath projectId,"/views/",toPath view,"/buckets/",toPath bucketId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsProjectIDViewsViewBucketsBucketIDDelete  
-- | @application/json@
instance Produces ProjectsProjectIDViewsViewBucketsBucketIDDelete MimeJSON


-- *** projectsProjectIDViewsViewBucketsBucketIDPost

-- | @POST \/projects\/{projectID}\/views\/{view}\/buckets\/{bucketID}@
-- 
-- Update an existing bucket
-- 
-- Updates an existing kanban bucket.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsProjectIDViewsViewBucketsBucketIDPost
  :: (Consumes ProjectsProjectIDViewsViewBucketsBucketIDPost MimeJSON, MimeRender MimeJSON ModelsBucket)
  => ModelsBucket -- ^ "bucket" -  The bucket object
  -> ProjectId -- ^ "projectId" -  Project Id
  -> BucketId -- ^ "bucketId" -  Bucket Id
  -> View -- ^ "view" -  Project view ID
  -> VikunjaRequest ProjectsProjectIDViewsViewBucketsBucketIDPost MimeJSON ModelsBucket MimeJSON
projectsProjectIDViewsViewBucketsBucketIDPost bucket (ProjectId projectId) (BucketId bucketId) (View view) =
  _mkRequest "POST" ["/projects/",toPath projectId,"/views/",toPath view,"/buckets/",toPath bucketId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` bucket

data ProjectsProjectIDViewsViewBucketsBucketIDPost 

-- | /Body Param/ "bucket" - The bucket object
instance HasBodyParam ProjectsProjectIDViewsViewBucketsBucketIDPost ModelsBucket 

-- | @application/json@
instance Consumes ProjectsProjectIDViewsViewBucketsBucketIDPost MimeJSON

-- | @application/json@
instance Produces ProjectsProjectIDViewsViewBucketsBucketIDPost MimeJSON


-- *** projectsProjectViewsGet

-- | @GET \/projects\/{project}\/views@
-- 
-- Get all project views for a project
-- 
-- Returns all project views for a sepcific project
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsProjectViewsGet
  :: Project -- ^ "project" -  Project ID
  -> VikunjaRequest ProjectsProjectViewsGet MimeNoContent [ModelsProjectView] MimeJSON
projectsProjectViewsGet (Project project) =
  _mkRequest "GET" ["/projects/",toPath project,"/views"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsProjectViewsGet  
-- | @application/json@
instance Produces ProjectsProjectViewsGet MimeJSON


-- *** projectsProjectViewsIdDelete

-- | @DELETE \/projects\/{project}\/views\/{id}@
-- 
-- Delete a project view
-- 
-- Deletes a project view.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsProjectViewsIdDelete
  :: Project -- ^ "project" -  Project ID
  -> Id -- ^ "id" -  Project View ID
  -> VikunjaRequest ProjectsProjectViewsIdDelete MimeNoContent ModelsMessage MimeJSON
projectsProjectViewsIdDelete (Project project) (Id id) =
  _mkRequest "DELETE" ["/projects/",toPath project,"/views/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsProjectViewsIdDelete  
-- | @application/json@
instance Produces ProjectsProjectViewsIdDelete MimeJSON


-- *** projectsProjectViewsIdGet

-- | @GET \/projects\/{project}\/views\/{id}@
-- 
-- Get one project view
-- 
-- Returns a project view by its ID.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsProjectViewsIdGet
  :: Project -- ^ "project" -  Project ID
  -> Id -- ^ "id" -  Project View ID
  -> VikunjaRequest ProjectsProjectViewsIdGet MimeNoContent ModelsProjectView MimeJSON
projectsProjectViewsIdGet (Project project) (Id id) =
  _mkRequest "GET" ["/projects/",toPath project,"/views/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data ProjectsProjectViewsIdGet  
-- | @application/json@
instance Produces ProjectsProjectViewsIdGet MimeJSON


-- *** projectsProjectViewsIdPost

-- | @POST \/projects\/{project}\/views\/{id}@
-- 
-- Updates a project view
-- 
-- Updates a project view.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsProjectViewsIdPost
  :: (Consumes ProjectsProjectViewsIdPost MimeJSON, MimeRender MimeJSON ModelsProjectView)
  => ModelsProjectView -- ^ "view" -  The project view with updated values you want to change.
  -> Project -- ^ "project" -  Project ID
  -> Id -- ^ "id" -  Project View ID
  -> VikunjaRequest ProjectsProjectViewsIdPost MimeJSON ModelsProjectView MimeJSON
projectsProjectViewsIdPost view (Project project) (Id id) =
  _mkRequest "POST" ["/projects/",toPath project,"/views/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` view

data ProjectsProjectViewsIdPost 

-- | /Body Param/ "view" - The project view with updated values you want to change.
instance HasBodyParam ProjectsProjectViewsIdPost ModelsProjectView 

-- | @application/json@
instance Consumes ProjectsProjectViewsIdPost MimeJSON

-- | @application/json@
instance Produces ProjectsProjectViewsIdPost MimeJSON


-- *** projectsProjectViewsPut

-- | @PUT \/projects\/{project}\/views@
-- 
-- Create a project view
-- 
-- Create a project view in a specific project.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsProjectViewsPut
  :: (Consumes ProjectsProjectViewsPut MimeJSON, MimeRender MimeJSON ModelsProjectView)
  => ModelsProjectView -- ^ "view" -  The project view you want to create.
  -> Project -- ^ "project" -  Project ID
  -> VikunjaRequest ProjectsProjectViewsPut MimeJSON ModelsProjectView MimeJSON
projectsProjectViewsPut view (Project project) =
  _mkRequest "PUT" ["/projects/",toPath project,"/views"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` view

data ProjectsProjectViewsPut 

-- | /Body Param/ "view" - The project view you want to create.
instance HasBodyParam ProjectsProjectViewsPut ModelsProjectView 

-- | @application/json@
instance Consumes ProjectsProjectViewsPut MimeJSON

-- | @application/json@
instance Produces ProjectsProjectViewsPut MimeJSON


-- *** projectsPut

-- | @PUT \/projects@
-- 
-- Creates a new project
-- 
-- Creates a new project. If a parent project is provided the user needs to have write access to that project.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
projectsPut
  :: (Consumes ProjectsPut MimeJSON, MimeRender MimeJSON ModelsProject)
  => ModelsProject -- ^ "project" -  The project you want to create.
  -> VikunjaRequest ProjectsPut MimeJSON ModelsProject MimeJSON
projectsPut project =
  _mkRequest "PUT" ["/projects"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` project

data ProjectsPut 

-- | /Body Param/ "project" - The project you want to create.
instance HasBodyParam ProjectsPut ModelsProject 

-- | @application/json@
instance Consumes ProjectsPut MimeJSON

-- | @application/json@
instance Produces ProjectsPut MimeJSON

