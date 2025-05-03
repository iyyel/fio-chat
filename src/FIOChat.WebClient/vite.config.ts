import { defineConfig } from 'vite'

export default defineConfig({
    clearScreen: false,
    server: {
        watch: {
            ignored: [
                "*.fs" // Don't watch F# files
            ]
        }
    }
})