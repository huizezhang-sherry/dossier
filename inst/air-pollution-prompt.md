# Task

You will read a PDF file and extract structured data only from the Methods section. The output should be in JSON format, wrapped in a JSON code block, using the top-level key "decisions". Each item in "decisions" should be a dictionary containing the following fields:

- `model`: the main model used to model mortality vs. air pollution (e.g., "generalized additive model", "distributed lag model"). Should be one model per paper.
- `variable`: the variable the statistical method is applied to.
- `method`: standard statistical method applied (e.g., LOESS, spline). If the `type` is "spatial" or "temporal", write "NA". 
- `parameter`: the parameter of the statistical method discussed (e.g., degrees of freedom, number of knots). If the `type` is "spatial" or "temporal", write "NA". 
- `type`: one of "parameter", "spatial", or "temporal", indicating the nature of the decision. 
- `reason`: the reason for the decision made. If none is given, write "NA".
- `decision`: the decision made regarding the parameter, spatial, or temporal aspect. If not specified, write "NA".
- `reference`: if the reason for the decision has any reference, find the corresponding reference in the reference section and write the reference in the following format: abc, where a is the Last name of first author (wiithout accent), b is the year in four digit format, and c is the first word of the title, excluding a/an/the, e.g. braga2001lag.

## Rules

* When a standard statistical method (`method`) is identified on a variable, look for the discussion of its parameter choice (e.g. degree of freedom, span, smoothing parameter for splines). If no details found, write "NA" for `parameter`, "parameter" for `type`, "NA" for `reason`, and "NA" for `decision`.

* Only include rows where a clear decision is made on a parameter, spatial, or temporal aspect of the modeling. For ambiguous or vague decisions (e.g., "not below 2 months"), do not record a row.

* Ignore general narrative text or rationale for including a variable. Only capture decisions and reasons relevant to how the parameter in the method is chosen and how the variable is processed spatially or temporally. For example
	* invalid reasons include "to capture delayed temperature effect", "to establish trend", "to adjust for temporal trend", "to examine delayed effects". This type of reasons don't provide specific justification for why the specific lag values (e.g. same day and previous day) are considered as decisison. Write "NA" if cannot find the justification.

* Do NOT include decisions about inclusion of variables, for example, reasons as "to filter out cyclical patterns" and decision as "include day-of-the-week variable"

* A single sentence may include multiple decisions. Split them into separate rows.

* If a parameter decision also includes how it is implemented spatially or temporally, split this into separate "spatial" and/or "temporal" rows with the same method and parameter fields.

* For "model"-level decisions, set: `variable` = `model`, `method` = `NA`, `parameter` = `NA`, `type` = `spatial` or `temporal` depending on the decision context

* A paper may contain multiple models; label them precisely (e.g., "generalized additive Poisson regression", "lag distributed model").

* Include a citation-based reasons when given

* Ensure that the variable column contains specific, measurable variables, not general terms (e.g. "weather"). If the text refers to broad categories (e.g., "weather"), go back in the document to identify the specific variables involved (e.g. temperature, relative humidity, dew point temperature, etc)

## Special Notes

* Treat **temperature** and **dewpoint temperature** as separate variables.
* treat **maximum temperature** and **minimum temperature** as separate variables.


