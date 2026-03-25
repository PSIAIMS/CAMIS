function adjustIframeHeight(iframe) {
    const iframeDocument = iframe.contentDocument || iframe.contentWindow.document;
    if (!iframeDocument) return;

    // Temporarily force horizontal scroll to measure its size
    const hasHorizontalScroll = iframeDocument.documentElement.scrollWidth > iframeDocument.documentElement.clientWidth;
    const scrollbarHeight = hasHorizontalScroll ? 17 : 0; // Approx height of horizontal scrollbar

    // Adjust height to content's scrollHeight minus scrollbar if needed
    iframe.style.height = (iframeDocument.documentElement.scrollHeight + scrollbarHeight) + 'px';
}

function adjustAllIframes() {
    document.querySelectorAll('.resizable-iframe').forEach((iframe) => {
        adjustIframeHeight(iframe);
    });
}

// Adjust height when iframe is loaded
document.querySelectorAll('.resizable-iframe').forEach((iframe) => {
    iframe.addEventListener('load', () => adjustIframeHeight(iframe));
});

// Re-adjust on window resize
window.addEventListener('resize', adjustAllIframes);
window.addEventListener('load', adjustAllIframes);
