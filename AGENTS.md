# Dashboard delivery instructions

After dashboard changes, run the relevant checks, commit the changes, push them to the user's configured GitHub repository, and give the user a direct commit link. The user explicitly requested this workflow on 2026-10-20. If no GitHub remote or authentication is configured, request the missing repository information and report that the push is pending; never claim an unverified push. Do not force-push or include secrets, session files, source datasets, or generated exports without checking their intended scope.

GitHub repository: https://github.com/Gad-Armel/Business_Operations_Intelligence_Dashboard
Remote: origin. Default branch: main. Preserve existing history and report the pushed commit URL after verifying the remote SHA.
