{-
   Vikunja API

   # Pagination Every endpoint capable of pagination will return two headers: * `x-pagination-total-pages`: The total number of available pages for this request * `x-pagination-result-count`: The number of items returned for this request. # Rights All endpoints which return a single item (project, task, etc.) - no array - will also return a `x-max-right` header with the max right the user has on this item as an int where `0` is `Read Only`, `1` is `Read & Write` and `2` is `Admin`. This can be used to show or hide ui elements based on the rights the user has. # Errors All errors have an error code and a human-readable error message in addition to the http status code. You should always check for the status code in the response, not only the http status code. Due to limitations in the swagger library we're using for this document, only one error per http status code is documented here. Make sure to check the [error docs](https://vikunja.io/docs/errors/) in Vikunja's documentation for a full list of available error codes. # Authorization **JWT-Auth:** Main authorization method, used for most of the requests. Needs `Authorization: Bearer <jwt-token>`-header to authenticate successfully.  **API Token:** You can create scoped API tokens for your user and use the token to make authenticated requests in the context of that user. The token must be provided via an `Authorization: Bearer <token>` header, similar to jwt auth. See the documentation for the `api` group to manage token creation and revocation.  **BasicAuth:** Only used when requesting tasks via CalDAV. <!-- ReDoc-Inject: <security-definitions> -->

   OpenAPI Version: 3.0.1
   Vikunja API API version: 0.24.6
   Contact: hello@vikunja.io
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Vikunja.API.User
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Vikunja.API.User where

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


-- ** User

-- *** userConfirmPost

-- | @POST \/user\/confirm@
-- 
-- Confirm the email of a new user
-- 
-- Confirms the email of a newly registered user.
-- 
userConfirmPost
  :: (Consumes UserConfirmPost MimeJSON, MimeRender MimeJSON UserEmailConfirm)
  => UserEmailConfirm -- ^ "credentials" -  The token.
  -> VikunjaRequest UserConfirmPost MimeJSON ModelsMessage MimeJSON
userConfirmPost credentials =
  _mkRequest "POST" ["/user/confirm"]
    `setBodyParam` credentials

data UserConfirmPost 

-- | /Body Param/ "credentials" - The token.
instance HasBodyParam UserConfirmPost UserEmailConfirm 

-- | @application/json@
instance Consumes UserConfirmPost MimeJSON

-- | @application/json@
instance Produces UserConfirmPost MimeJSON


-- *** userDeletionCancelPost

-- | @POST \/user\/deletion\/cancel@
-- 
-- Abort a user deletion request
-- 
-- Aborts an in-progress user deletion.
-- 
userDeletionCancelPost
  :: (Consumes UserDeletionCancelPost MimeJSON, MimeRender MimeJSON V1UserPasswordConfirmation)
  => V1UserPasswordConfirmation -- ^ "credentials" -  The user password to confirm.
  -> VikunjaRequest UserDeletionCancelPost MimeJSON ModelsMessage MimeJSON
userDeletionCancelPost credentials =
  _mkRequest "POST" ["/user/deletion/cancel"]
    `setBodyParam` credentials

data UserDeletionCancelPost 

-- | /Body Param/ "credentials" - The user password to confirm.
instance HasBodyParam UserDeletionCancelPost V1UserPasswordConfirmation 

-- | @application/json@
instance Consumes UserDeletionCancelPost MimeJSON

-- | @application/json@
instance Produces UserDeletionCancelPost MimeJSON


-- *** userDeletionConfirmPost

-- | @POST \/user\/deletion\/confirm@
-- 
-- Confirm a user deletion request
-- 
-- Confirms the deletion request of a user sent via email.
-- 
userDeletionConfirmPost
  :: (Consumes UserDeletionConfirmPost MimeJSON, MimeRender MimeJSON V1UserDeletionRequestConfirm)
  => V1UserDeletionRequestConfirm -- ^ "credentials" -  The token.
  -> VikunjaRequest UserDeletionConfirmPost MimeJSON ModelsMessage MimeJSON
userDeletionConfirmPost credentials =
  _mkRequest "POST" ["/user/deletion/confirm"]
    `setBodyParam` credentials

data UserDeletionConfirmPost 

-- | /Body Param/ "credentials" - The token.
instance HasBodyParam UserDeletionConfirmPost V1UserDeletionRequestConfirm 

-- | @application/json@
instance Consumes UserDeletionConfirmPost MimeJSON

-- | @application/json@
instance Produces UserDeletionConfirmPost MimeJSON


-- *** userDeletionRequestPost

-- | @POST \/user\/deletion\/request@
-- 
-- Request the deletion of the user
-- 
-- Requests the deletion of the current user. It will trigger an email which has to be confirmed to start the deletion.
-- 
userDeletionRequestPost
  :: (Consumes UserDeletionRequestPost MimeJSON, MimeRender MimeJSON V1UserPasswordConfirmation)
  => V1UserPasswordConfirmation -- ^ "credentials" -  The user password.
  -> VikunjaRequest UserDeletionRequestPost MimeJSON ModelsMessage MimeJSON
userDeletionRequestPost credentials =
  _mkRequest "POST" ["/user/deletion/request"]
    `setBodyParam` credentials

data UserDeletionRequestPost 

-- | /Body Param/ "credentials" - The user password.
instance HasBodyParam UserDeletionRequestPost V1UserPasswordConfirmation 

-- | @application/json@
instance Consumes UserDeletionRequestPost MimeJSON

-- | @application/json@
instance Produces UserDeletionRequestPost MimeJSON


-- *** userExportDownloadPost

-- | @POST \/user\/export\/download@
-- 
-- Download a user data export.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userExportDownloadPost
  :: (Consumes UserExportDownloadPost MimeJSON, MimeRender MimeJSON V1UserPasswordConfirmation)
  => V1UserPasswordConfirmation -- ^ "password" -  User password to confirm the download.
  -> VikunjaRequest UserExportDownloadPost MimeJSON ModelsMessage MimeJSON
userExportDownloadPost password =
  _mkRequest "POST" ["/user/export/download"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` password

data UserExportDownloadPost 

-- | /Body Param/ "password" - User password to confirm the download.
instance HasBodyParam UserExportDownloadPost V1UserPasswordConfirmation 

-- | @application/json@
instance Consumes UserExportDownloadPost MimeJSON

-- | @application/json@
instance Produces UserExportDownloadPost MimeJSON


-- *** userExportRequestPost

-- | @POST \/user\/export\/request@
-- 
-- Request a user data export.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userExportRequestPost
  :: (Consumes UserExportRequestPost MimeJSON, MimeRender MimeJSON V1UserPasswordConfirmation)
  => V1UserPasswordConfirmation -- ^ "password" -  User password to confirm the data export request.
  -> VikunjaRequest UserExportRequestPost MimeJSON ModelsMessage MimeJSON
userExportRequestPost password =
  _mkRequest "POST" ["/user/export/request"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` password

data UserExportRequestPost 

-- | /Body Param/ "password" - User password to confirm the data export request.
instance HasBodyParam UserExportRequestPost V1UserPasswordConfirmation 

-- | @application/json@
instance Consumes UserExportRequestPost MimeJSON

-- | @application/json@
instance Produces UserExportRequestPost MimeJSON


-- *** userGet

-- | @GET \/user@
-- 
-- Get user information
-- 
-- Returns the current user object with their settings.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userGet
  :: VikunjaRequest UserGet MimeNoContent V1UserWithSettings MimeJSON
userGet =
  _mkRequest "GET" ["/user"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data UserGet  
-- | @application/json@
instance Produces UserGet MimeJSON


-- *** userPasswordPost

-- | @POST \/user\/password@
-- 
-- Change password
-- 
-- Lets the current user change its password.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userPasswordPost
  :: (Consumes UserPasswordPost MimeJSON, MimeRender MimeJSON V1UserPassword)
  => V1UserPassword -- ^ "userPassword" -  The current and new password.
  -> VikunjaRequest UserPasswordPost MimeJSON ModelsMessage MimeJSON
userPasswordPost userPassword =
  _mkRequest "POST" ["/user/password"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` userPassword

data UserPasswordPost 

-- | /Body Param/ "userPassword" - The current and new password.
instance HasBodyParam UserPasswordPost V1UserPassword 

-- | @application/json@
instance Consumes UserPasswordPost MimeJSON

-- | @application/json@
instance Produces UserPasswordPost MimeJSON


-- *** userPasswordResetPost

-- | @POST \/user\/password\/reset@
-- 
-- Resets a password
-- 
-- Resets a user email with a previously reset token.
-- 
userPasswordResetPost
  :: (Consumes UserPasswordResetPost MimeJSON, MimeRender MimeJSON UserPasswordReset)
  => UserPasswordReset -- ^ "credentials" -  The token with the new password.
  -> VikunjaRequest UserPasswordResetPost MimeJSON ModelsMessage MimeJSON
userPasswordResetPost credentials =
  _mkRequest "POST" ["/user/password/reset"]
    `setBodyParam` credentials

data UserPasswordResetPost 

-- | /Body Param/ "credentials" - The token with the new password.
instance HasBodyParam UserPasswordResetPost UserPasswordReset 

-- | @application/json@
instance Consumes UserPasswordResetPost MimeJSON

-- | @application/json@
instance Produces UserPasswordResetPost MimeJSON


-- *** userPasswordTokenPost

-- | @POST \/user\/password\/token@
-- 
-- Request password reset token
-- 
-- Requests a token to reset a users password. The token is sent via email.
-- 
userPasswordTokenPost
  :: (Consumes UserPasswordTokenPost MimeJSON, MimeRender MimeJSON UserPasswordTokenRequest)
  => UserPasswordTokenRequest -- ^ "credentials" -  The username of the user to request a token for.
  -> VikunjaRequest UserPasswordTokenPost MimeJSON ModelsMessage MimeJSON
userPasswordTokenPost credentials =
  _mkRequest "POST" ["/user/password/token"]
    `setBodyParam` credentials

data UserPasswordTokenPost 

-- | /Body Param/ "credentials" - The username of the user to request a token for.
instance HasBodyParam UserPasswordTokenPost UserPasswordTokenRequest 

-- | @application/json@
instance Consumes UserPasswordTokenPost MimeJSON

-- | @application/json@
instance Produces UserPasswordTokenPost MimeJSON


-- *** userSettingsAvatarGet

-- | @GET \/user\/settings\/avatar@
-- 
-- Return user avatar setting
-- 
-- Returns the current user's avatar setting.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsAvatarGet
  :: VikunjaRequest UserSettingsAvatarGet MimeNoContent V1UserAvatarProvider MimeJSON
userSettingsAvatarGet =
  _mkRequest "GET" ["/user/settings/avatar"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data UserSettingsAvatarGet  
-- | @application/json@
instance Produces UserSettingsAvatarGet MimeJSON


-- *** userSettingsAvatarPost

-- | @POST \/user\/settings\/avatar@
-- 
-- Set the user's avatar
-- 
-- Changes the user avatar. Valid types are gravatar (uses the user email), upload, initials, default.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsAvatarPost
  :: (Consumes UserSettingsAvatarPost MimeJSON, MimeRender MimeJSON V1UserAvatarProvider)
  => V1UserAvatarProvider -- ^ "avatar" -  The user's avatar setting
  -> VikunjaRequest UserSettingsAvatarPost MimeJSON ModelsMessage MimeJSON
userSettingsAvatarPost avatar =
  _mkRequest "POST" ["/user/settings/avatar"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` avatar

data UserSettingsAvatarPost 

-- | /Body Param/ "avatar" - The user's avatar setting
instance HasBodyParam UserSettingsAvatarPost V1UserAvatarProvider 

-- | @application/json@
instance Consumes UserSettingsAvatarPost MimeJSON

-- | @application/json@
instance Produces UserSettingsAvatarPost MimeJSON


-- *** userSettingsAvatarUploadPut

-- | @PUT \/user\/settings\/avatar\/upload@
-- 
-- Upload a user avatar
-- 
-- Upload a user avatar. This will also set the user's avatar provider to \"upload\"
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsAvatarUploadPut
  :: (Consumes UserSettingsAvatarUploadPut MimeMultipartFormData)
  => Avatar -- ^ "avatar" -  The avatar as single file.
  -> VikunjaRequest UserSettingsAvatarUploadPut MimeMultipartFormData ModelsMessage MimeJSON
userSettingsAvatarUploadPut (Avatar avatar) =
  _mkRequest "PUT" ["/user/settings/avatar/upload"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `_addMultiFormPart` NH.partLBS "avatar" (mimeRender' MimeMultipartFormData avatar)

data UserSettingsAvatarUploadPut  

-- | @multipart/form-data@
instance Consumes UserSettingsAvatarUploadPut MimeMultipartFormData

-- | @application/json@
instance Produces UserSettingsAvatarUploadPut MimeJSON


-- *** userSettingsEmailPost

-- | @POST \/user\/settings\/email@
-- 
-- Update email address
-- 
-- Lets the current user change their email address.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsEmailPost
  :: (Consumes UserSettingsEmailPost MimeJSON, MimeRender MimeJSON UserEmailUpdate)
  => UserEmailUpdate -- ^ "userEmailUpdate" -  The new email address and current password.
  -> VikunjaRequest UserSettingsEmailPost MimeJSON ModelsMessage MimeJSON
userSettingsEmailPost userEmailUpdate =
  _mkRequest "POST" ["/user/settings/email"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` userEmailUpdate

data UserSettingsEmailPost 

-- | /Body Param/ "userEmailUpdate" - The new email address and current password.
instance HasBodyParam UserSettingsEmailPost UserEmailUpdate 

-- | @application/json@
instance Consumes UserSettingsEmailPost MimeJSON

-- | @application/json@
instance Produces UserSettingsEmailPost MimeJSON


-- *** userSettingsGeneralPost

-- | @POST \/user\/settings\/general@
-- 
-- Change general user settings of the current user.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsGeneralPost
  :: (Consumes UserSettingsGeneralPost MimeJSON, MimeRender MimeJSON V1UserSettings)
  => V1UserSettings -- ^ "avatar" -  The updated user settings
  -> VikunjaRequest UserSettingsGeneralPost MimeJSON ModelsMessage MimeJSON
userSettingsGeneralPost avatar =
  _mkRequest "POST" ["/user/settings/general"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` avatar

data UserSettingsGeneralPost 

-- | /Body Param/ "avatar" - The updated user settings
instance HasBodyParam UserSettingsGeneralPost V1UserSettings 

-- | @application/json@
instance Consumes UserSettingsGeneralPost MimeJSON

-- | @application/json@
instance Produces UserSettingsGeneralPost MimeJSON


-- *** userSettingsTokenCaldavGet

-- | @GET \/user\/settings\/token\/caldav@
-- 
-- Returns the caldav tokens for the current user
-- 
-- Return the IDs and created dates of all caldav tokens for the current user.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsTokenCaldavGet
  :: VikunjaRequest UserSettingsTokenCaldavGet MimeNoContent [UserToken] MimeJSON
userSettingsTokenCaldavGet =
  _mkRequest "GET" ["/user/settings/token/caldav"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data UserSettingsTokenCaldavGet  
-- | @application/json@
instance Produces UserSettingsTokenCaldavGet MimeJSON


-- *** userSettingsTokenCaldavIdGet

-- | @GET \/user\/settings\/token\/caldav\/{id}@
-- 
-- Delete a caldav token by id
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsTokenCaldavIdGet
  :: Id -- ^ "id" -  Token ID
  -> VikunjaRequest UserSettingsTokenCaldavIdGet MimeNoContent ModelsMessage MimeJSON
userSettingsTokenCaldavIdGet (Id id) =
  _mkRequest "GET" ["/user/settings/token/caldav/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data UserSettingsTokenCaldavIdGet  
-- | @application/json@
instance Produces UserSettingsTokenCaldavIdGet MimeJSON


-- *** userSettingsTokenCaldavPut

-- | @PUT \/user\/settings\/token\/caldav@
-- 
-- Generate a caldav token
-- 
-- Generates a caldav token which can be used for the caldav api. It is not possible to see the token again after it was generated.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsTokenCaldavPut
  :: VikunjaRequest UserSettingsTokenCaldavPut MimeNoContent UserToken MimeJSON
userSettingsTokenCaldavPut =
  _mkRequest "PUT" ["/user/settings/token/caldav"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data UserSettingsTokenCaldavPut  
-- | @application/json@
instance Produces UserSettingsTokenCaldavPut MimeJSON


-- *** userSettingsTotpDisablePost

-- | @POST \/user\/settings\/totp\/disable@
-- 
-- Disable totp settings
-- 
-- Disables any totp settings for the current user.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsTotpDisablePost
  :: (Consumes UserSettingsTotpDisablePost MimeJSON, MimeRender MimeJSON UserLogin)
  => UserLogin -- ^ "totp" -  The current user's password (only password is enough).
  -> VikunjaRequest UserSettingsTotpDisablePost MimeJSON ModelsMessage MimeJSON
userSettingsTotpDisablePost totp =
  _mkRequest "POST" ["/user/settings/totp/disable"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` totp

data UserSettingsTotpDisablePost 

-- | /Body Param/ "totp" - The current user's password (only password is enough).
instance HasBodyParam UserSettingsTotpDisablePost UserLogin 

-- | @application/json@
instance Consumes UserSettingsTotpDisablePost MimeJSON

-- | @application/json@
instance Produces UserSettingsTotpDisablePost MimeJSON


-- *** userSettingsTotpEnablePost

-- | @POST \/user\/settings\/totp\/enable@
-- 
-- Enable a previously enrolled totp setting.
-- 
-- Enables a previously enrolled totp setting by providing a totp passcode.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsTotpEnablePost
  :: (Consumes UserSettingsTotpEnablePost MimeJSON, MimeRender MimeJSON UserTOTPPasscode)
  => UserTOTPPasscode -- ^ "totp" -  The totp passcode.
  -> VikunjaRequest UserSettingsTotpEnablePost MimeJSON ModelsMessage MimeJSON
userSettingsTotpEnablePost totp =
  _mkRequest "POST" ["/user/settings/totp/enable"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)
    `setBodyParam` totp

data UserSettingsTotpEnablePost 

-- | /Body Param/ "totp" - The totp passcode.
instance HasBodyParam UserSettingsTotpEnablePost UserTOTPPasscode 

-- | @application/json@
instance Consumes UserSettingsTotpEnablePost MimeJSON

-- | @application/json@
instance Produces UserSettingsTotpEnablePost MimeJSON


-- *** userSettingsTotpEnrollPost

-- | @POST \/user\/settings\/totp\/enroll@
-- 
-- Enroll a user into totp
-- 
-- Creates an initial setup for the user in the db. After this step, the user needs to verify they have a working totp setup with the \"enable totp\" endpoint.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsTotpEnrollPost
  :: VikunjaRequest UserSettingsTotpEnrollPost MimeNoContent UserTOTP MimeJSON
userSettingsTotpEnrollPost =
  _mkRequest "POST" ["/user/settings/totp/enroll"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data UserSettingsTotpEnrollPost  
-- | @application/json@
instance Produces UserSettingsTotpEnrollPost MimeJSON


-- *** userSettingsTotpGet

-- | @GET \/user\/settings\/totp@
-- 
-- Totp setting for the current user
-- 
-- Returns the current user totp setting or an error if it is not enabled.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsTotpGet
  :: VikunjaRequest UserSettingsTotpGet MimeNoContent UserTOTP MimeJSON
userSettingsTotpGet =
  _mkRequest "GET" ["/user/settings/totp"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data UserSettingsTotpGet  
-- | @application/json@
instance Produces UserSettingsTotpGet MimeJSON


-- *** userSettingsTotpQrcodeGet

-- | @GET \/user\/settings\/totp\/qrcode@
-- 
-- Totp QR Code
-- 
-- Returns a qr code for easier setup at end user's devices.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userSettingsTotpQrcodeGet
  :: VikunjaRequest UserSettingsTotpQrcodeGet MimeNoContent FilePath MimeJSON
userSettingsTotpQrcodeGet =
  _mkRequest "GET" ["/user/settings/totp/qrcode"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data UserSettingsTotpQrcodeGet  
-- | @application/json@
instance Produces UserSettingsTotpQrcodeGet MimeJSON


-- *** userTimezonesGet

-- | @GET \/user\/timezones@
-- 
-- Get all available time zones on this vikunja instance
-- 
-- Because available time zones depend on the system Vikunja is running on, this endpoint returns a project of all valid time zones this particular Vikunja instance can handle. The project of time zones is not sorted, you should sort it on the client.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
userTimezonesGet
  :: VikunjaRequest UserTimezonesGet MimeNoContent [Text] MimeJSON
userTimezonesGet =
  _mkRequest "GET" ["/user/timezones"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data UserTimezonesGet  
-- | @application/json@
instance Produces UserTimezonesGet MimeJSON


-- *** userTokenPost

-- | @POST \/user\/token@
-- 
-- Renew user token
-- 
-- Returns a new valid jwt user token with an extended length.
-- 
userTokenPost
  :: VikunjaRequest UserTokenPost MimeNoContent AuthToken MimeJSON
userTokenPost =
  _mkRequest "POST" ["/user/token"]

data UserTokenPost  
-- | @application/json@
instance Produces UserTokenPost MimeJSON


-- *** usernameAvatarGet

-- | @GET \/{username}\/avatar@
-- 
-- User Avatar
-- 
-- Returns the user avatar as image.
-- 
usernameAvatarGet
  :: Username -- ^ "username" -  The username of the user who's avatar you want to get
  -> VikunjaRequest UsernameAvatarGet MimeNoContent FilePath MimeOctetStream
usernameAvatarGet (Username username) =
  _mkRequest "GET" ["/",toPath username,"/avatar"]

data UsernameAvatarGet  

-- | /Optional Param/ "size" - The size of the avatar you want to get. If bigger than the max configured size this will be adjusted to the maximum size.
instance HasOptionalParam UsernameAvatarGet Size where
  applyOptionalParam req (Size xs) =
    req `addQuery` toQuery ("size", Just xs)
-- | @application/octet-stream@
instance Produces UsernameAvatarGet MimeOctetStream


-- *** usersGet

-- | @GET \/users@
-- 
-- Get users
-- 
-- Search for a user by its username, name or full email. Name (not username) or email require that the user has enabled this in their settings.
-- 
-- AuthMethod: 'AuthApiKeyJWTKeyAuth'
-- 
usersGet
  :: VikunjaRequest UsersGet MimeNoContent [UserUser] MimeJSON
usersGet =
  _mkRequest "GET" ["/users"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyJWTKeyAuth)

data UsersGet  

-- | /Optional Param/ "s" - The search criteria.
instance HasOptionalParam UsersGet S where
  applyOptionalParam req (S xs) =
    req `addQuery` toQuery ("s", Just xs)
-- | @application/json@
instance Produces UsersGet MimeJSON

