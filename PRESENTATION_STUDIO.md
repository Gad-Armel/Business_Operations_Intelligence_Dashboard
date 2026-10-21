# Presentation Studio

Open **Analysis & Probability → Presentation Studio**. The generator lives in `R/presentation_generator.R`; it never changes source records.

1. Choose one of 12 story templates: executive, board, sales, profitability, customer, operations, supply chain, cash, risk, geography, investment, or data discovery.
2. Choose the audience, decision question, business context and presentation title.
3. Select the current Analysis Lab dataset (including its filters), a dashboard source using the global period, or a session CSV. Choose the measure, segment field, date field, units and aggregation. Optional dates further restrict the source. Optional targets must match the same period, measure and units.
4. Select summary, segment, monthly trend and quality analyses. Generate locally or select the AI narrative option.
5. Review evidence and proposed recommendations, edit slide headings, points and notes, then download the HTML slide deck or Markdown outline. The deck opens without an internet connection, supports arrow keys and browser Print to PDF, and contains chart values in expandable sections.

The 12 templates have different story outlines and accent palettes. Slide count adapts to available evidence and selected analyses. A trend needs a date field; segment analysis needs a segment field. Missing measure observations are excluded, exact duplicate rows are retained and reported, missing months are not imputed, and rankings display at most 12 segments. Negative values remain in the analysis. No causal relationship, forecast, ROI or business target is invented. Local recommendations are deliberately review-oriented; AI can tailor proposed actions to the selected brief.

## Optional AI connection

Configure `OPENAI_API_KEY` in the server environment, and optionally `AURELIS_PRESENTATION_MODEL` with a model your API account can access. Do not put credentials in source files. Choose AI mode, enter the model and select the summary-sharing checkbox. The server calls the OpenAI Responses API with `store = FALSE`; computed summaries, category names, the outline and your brief are sent, not raw table rows. Aggregated labels can still contain business-sensitive information, so review your selected data before sharing. Configuration errors, timeouts and invalid output leave the last generated deck unchanged. No API request is made in local mode.

The engine instructs AI to ground claims in supplied evidence and validates the output structure. This is not automatic factual verification: review statements and proposed recommendations before delivering them. API errors are reported without exposing credentials or response bodies. AI output is rendered as escaped text. Configuration does not imply that a live API call has been tested.

Preview and downloads use the last generated snapshot. A notice appears if the source or settings change. Regenerating replaces the draft, including manual edits. Current exports are HTML and Markdown; PDF uses the browser print dialog. Native PowerPoint export is not included.

## Checks

Run `Rscript tests/validate_presentation.R` and `Rscript tests/smoke_app.R`. The presentation tests cover all templates, aggregation, negative/missing values, focus selection, HTML escaping, mocked AI output, editing, stale snapshots and date filtering. No paid API call is required.

API implementation reference: https://developers.openai.com/api/docs/guides/text
