#!/usr/bin/env python3
from __future__ import annotations

from omni_lsp_shared import *
from omni_lsp_actions_diagnostics import ActionsDiagnosticsMixin
from omni_lsp_core import CoreMixin
from omni_lsp_format_helpers import FormatHelpersMixin
from omni_lsp_hover_completion import HoverCompletionMixin
from omni_lsp_links_codelens import LinksCodeLensMixin
from omni_lsp_navigation import NavigationMixin
from omni_lsp_parser_io import ParserIOMixin
from omni_lsp_presentation import PresentationMixin
from omni_lsp_workspace import WorkspaceMixin


class OmniLspServer(
    CoreMixin,
    HoverCompletionMixin,
    PresentationMixin,
    LinksCodeLensMixin,
    FormatHelpersMixin,
    WorkspaceMixin,
    ActionsDiagnosticsMixin,
    NavigationMixin,
    ParserIOMixin,
):
    pass


def main() -> int:
    return OmniLspServer().serve()


if __name__ == "__main__":
    raise SystemExit(main())
