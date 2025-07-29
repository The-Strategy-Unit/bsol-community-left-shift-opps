#' Get Azure Token
#'
#' This function retrieves an Azure token for a specified resource.
#' It first attempts to get a managed token.
#' If that fails, it tries to retrieve a user token using the provided
#' parameters.
#'
#' @param resource A string specifying the Azure resource for which the token
#'  is requested. Defaults to `"https://storage.azure.com"`.
#' @param tenant A string specifying the Azure tenant. Defaults to `"common"`.
#' @param app A string specifying the application ID (client ID). If `NULL`,
#'  the function attempts to retrieve the app ID from the AzureRMR token or
#'  prompts the user to log in to retrieve it.
#' @param auth_type A string specifying the authentication type. Defaults to
#'  `"device_code"`.
#'
#' @returns An Azure token object. If a managed token is available, it is
#'  returned. Otherwise, a user token is retrieved.
#' @examples
#' \dontrun{
#' # Get a token for the default resource
#' token <- get_azure_token()
#'
#' # Get a token for a specific resource and tenant
#' token <- get_azure_token(
#'  resource = "https://graph.microsoft.com",
#'  tenant = "my-tenant-id"
#' )
#'
#' # Get a token using a specific app ID
#' token <- get_azure_token(app = "my-app-id")
#' }
get_azure_token <- function(
  resource = "https://storage.azure.com",
  tenant = "common",
  app = NULL,
  auth_type = "device_code"
) {
  pluck_azure_id <- function() {
    purrr::pluck(AzureRMR::get_azure_login(), "token", "client", "client_id")
  }
  tryCatch(
    # try to get a managed token from the resource
    AzureAuth::get_managed_token(resource),
    # if that fails, try to get a user token
    error = function(e) {
      # find the token for the resource
      tokens <- try(
        AzureRMR::list_azure_tokens(),
        silent = TRUE
      )
      if (inherits(tokens, "try-error")) {
        # get a new token
        # if app is not specified, try to get the app ID from the AzureRMR token
        if (is.null(app)) {
          # if the user has previously logged in, we can use their app ID
          app <- try(pluck_azure_id(), silent = TRUE)
          # if not, log in, then get the app ID
          if (inherits(app, "try-error")) {
            AzureRMR::create_azure_login()
            app <- pluck_azure_id()
          }
        }
        token <- AzureRMR::get_azure_token(
          resource,
          tenant = tenant,
          app = app,
          auth_type = auth_type
        )
      } else {
        resources <- purrr::map(tokens, "resource")
        token_use <- match(resource, resources)[1]
        # if we have a token for the resource, use it
        if (!is.na(token_use)) {
          token <- tokens[[token_use]]
        } else {
          token <- NULL
        }
      }
      token
    }
  )
}


# old / original code ----------------------------------------------------

# from https://github.com/The-Strategy-Unit/su.azure/blob/main/R/container.R
get_container <- function(
  tenant = Sys.getenv("AZ_TENANT_ID"),
  app_id = Sys.getenv("AZ_APP_ID"),
  ep_uri = Sys.getenv("AZ_STORAGE_EP"),
  container_name = Sys.getenv("AZ_STORAGE_CONTAINER")
) {
  # if the app_id variable is empty, we assume that this is running on an Azure VM,
  # and then we will use Managed Identities for authentication.
  token <- if (app_id != "") {
    AzureAuth::get_azure_token(
      resource = "https://storage.azure.com",
      tenant = Sys.getenv("AZ_TENANT_ID"),
      app = app_id,
      auth_type = "device_code"
    )
  } else {
    AzureAuth::get_managed_token("https://storage.azure.com/") |>
      AzureAuth::extract_jwt()
  }

  ep_uri |>
    AzureStor::blob_endpoint(token = token) |>
    AzureStor::storage_container(container_name)
}


# from https://github.com/The-Strategy-Unit/nhp_project_information/issues/199
azure_auth_token <- function(
  resource = "https://storage.azure.com",
  tenant = "common",
  app = NULL,
  auth_type = "device_code"
) {
  tryCatch(
    AzureAuth::get_managed_token(resource),
    error = function(err) {
      if (is.null(app)) {
        app <- try(
          AzureRMR::get_azure_login()$token$client$client_id,
          silent = TRUE
        )
        if (inherits(app, "try-error")) {
          AzureRMR::create_azure_login()
          app <- AzureRMR::get_azure_login()$token$client$client_id
        }
      }
      tokens <- AzureRMR::list_azure_tokens()
      resources <- purrr::map(tokens, \(x) x$resource)
      token_use <- match(resource, resources)[1]
      if (!is.na(token_use)) {
        tokens[[token_use]]
      } else {
        AzureRMR::get_azure_token(
          resource,
          tenant = tenant,
          app = app,
          auth_type = auth_type
        )
      }
    }
  )
}
