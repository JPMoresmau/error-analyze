# Introduction

error-analyze is a small project that parses GHC and Cabal errors and tries to give causes for the error, so that an IDE could offer automatic resolutions. Hopefully one day GHC will expose errors in a typed structure and this won't be needed.

The goal is to have in a Haskell library the error message analysis code that was written in Java inside EclipseFP to power the EclipseFP quick fixes. Hopefully this can be useful to other projects.

# Usage

    import Language.Haskell.ErrorAnalyze

    let causes = errorCauses "my ugly GHC message"
    
Look at the `Language.Haskell.ErrorAnalyze` for the `ErrorCause` data type to find all the possible causes we can extract.

